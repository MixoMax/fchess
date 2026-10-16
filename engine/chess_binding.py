"""
chess_binding.py - Python bindings for the Fortran 2008 bitboard chess engine.

Requires the shared library built by engine/build.sh (bin/libfchess.so).
Set FCHESS_LIB=/path/to/libfchess.so to load it from anywhere.

Example
-------
    from chess_binding import Board

    board = Board()
    board.push("e2e4")
    board.push("e7e5")
    print(board)
    print([m.uci for m in board.legal_moves()])

    result = board.search(depth=5)
    print(result.best_move, result.score, result.nodes)
    board.push(result.best_move)
"""

from __future__ import annotations

import os
import threading
from ctypes import (CDLL, POINTER, byref, c_char, c_char_p, c_int, c_int32,
                    c_int64, create_string_buffer)
from dataclasses import dataclass
from enum import IntEnum
from typing import List, Optional, Union

__all__ = [
    "STARTING_FEN", "MATE_SCORE", "MATE_THRESHOLD", "MAX_MOVES",
    "Color", "PieceType", "Move", "Board", "PerftResult", "SearchResult",
    "square_name", "clear_transposition_table",
    "EngineError", "InvalidFenError", "IllegalMoveError",
]

STARTING_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
MAX_MOVES = 512
MATE_SCORE = 29000
MATE_THRESHOLD = 28000
_FEN_BUF = 256

# Move encoding flags (must match chess_engine.f90)
_MF_CAPTURE = 1 << 15
_MF_EP = 1 << 16
_MF_CASTLE = 1 << 17


class EngineError(Exception):
    """Base class for engine errors."""


class InvalidFenError(EngineError, ValueError):
    """The FEN string could not be parsed."""


class IllegalMoveError(EngineError, ValueError):
    """The move is not legal in the current position."""


# Library loading

def _find_library() -> str:
    env = os.environ.get("FCHESS_LIB")
    if env:
        if os.path.exists(env):
            return os.path.abspath(env)
        raise FileNotFoundError(f"FCHESS_LIB points to a missing file: {env}")

    here = os.path.dirname(os.path.abspath(__file__))
    candidates = (
        os.path.join(here, "libfchess.so"),
        os.path.join(here, "bin", "libfchess.so"),
        os.path.join(here, "engine", "bin", "libfchess.so"),
        os.path.join(here, "..", "engine", "bin", "libfchess.so"),
    )
    for candidate in candidates:
        if os.path.exists(candidate):
            return os.path.abspath(candidate)
    raise FileNotFoundError(
        "libfchess.so not found. Build it with `sh engine/build.sh` "
        "or set the FCHESS_LIB environment variable."
    )


def _declare_signatures(lib: CDLL) -> None:
    lib.chess_init.argtypes = []
    lib.chess_init.restype = None

    lib.chess_clear_tt.argtypes = []
    lib.chess_clear_tt.restype = None

    lib.chess_perft.argtypes = [c_char_p, c_int, POINTER(c_int64)]
    lib.chess_perft.restype = c_int

    lib.chess_legal_moves.argtypes = [c_char_p, POINTER(c_int32), c_int]
    lib.chess_legal_moves.restype = c_int

    lib.chess_eval.argtypes = [c_char_p, POINTER(c_int32)]
    lib.chess_eval.restype = c_int

    lib.chess_search.argtypes = [c_char_p, c_int, POINTER(c_int32),
                                 POINTER(c_int32), POINTER(c_int64)]
    lib.chess_search.restype = c_int

    lib.chess_make_move.argtypes = [c_char_p, c_int32, POINTER(c_char), c_int]
    lib.chess_make_move.restype = c_int

    lib.chess_in_check.argtypes = [c_char_p]
    lib.chess_in_check.restype = c_int


_lib = CDLL(_find_library())
_declare_signatures(_lib)
_lib.chess_init()

# The Fortran engine keeps global state (transposition table, tables), so all
# calls are serialised through this lock.
_lock = threading.Lock()


def clear_transposition_table() -> None:
    """Clear the engine's global transposition table."""
    with _lock:
        _lib.chess_clear_tt()


# Enums and helpers

class Color(IntEnum):
    WHITE = 0
    BLACK = 1


class PieceType(IntEnum):
    PAWN = 1
    KNIGHT = 2
    BISHOP = 3
    ROOK = 4
    QUEEN = 5
    KING = 6


_PROMO_FROM_CODE = {
    2: PieceType.KNIGHT,
    3: PieceType.BISHOP,
    4: PieceType.ROOK,
    5: PieceType.QUEEN,
}
_PROMO_CHAR = {
    PieceType.KNIGHT: "n",
    PieceType.BISHOP: "b",
    PieceType.ROOK: "r",
    PieceType.QUEEN: "q",
}


def square_name(sq: int) -> str:
    """Engine square index (a1=0 ... h8=63) to algebraic name."""
    return "abcdefgh"[sq % 8] + str(sq // 8 + 1)


class Move:
    """A chess move. `code` is the engine's packed int32 encoding."""

    __slots__ = ("code",)

    def __init__(self, code: int) -> None:
        self.code = int(code)

    @property
    def from_square(self) -> int:
        return self.code & 63

    @property
    def to_square(self) -> int:
        return (self.code >> 6) & 63

    @property
    def promotion(self) -> Optional[PieceType]:
        return _PROMO_FROM_CODE.get((self.code >> 12) & 7)

    @property
    def is_capture(self) -> bool:
        return bool(self.code & _MF_CAPTURE)

    @property
    def is_en_passant(self) -> bool:
        return bool(self.code & _MF_EP)

    @property
    def is_castling(self) -> bool:
        return bool(self.code & _MF_CASTLE)

    @property
    def uci(self) -> str:
        s = square_name(self.from_square) + square_name(self.to_square)
        promo = self.promotion
        if promo is not None:
            s += _PROMO_CHAR[promo]
        return s

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Move) and other.code == self.code

    def __hash__(self) -> int:
        return hash(self.code)

    def __repr__(self) -> str:
        return f"Move('{self.uci}')"

    def __str__(self) -> str:
        return self.uci


@dataclass(frozen=True)
class PerftResult:
    nodes: int
    captures: int
    en_passant: int
    castles: int
    promotions: int
    checks: int


@dataclass(frozen=True)
class SearchResult:
    best_move: Optional[Move]
    score: int          # centipawns, from the side to move's perspective
    nodes: int
    depth: int

    @property
    def is_mate(self) -> bool:
        return abs(self.score) > MATE_THRESHOLD

    @property
    def mate_in(self) -> Optional[int]:
        """
        Full moves until mate. Positive: the side to move mates in N.
        Negative: the side to move is mated in N. None if no mate is found.
        """
        if self.score > MATE_THRESHOLD:
            return (MATE_SCORE - self.score + 1) // 2
        if self.score < -MATE_THRESHOLD:
            return -((MATE_SCORE + self.score) // 2)
        return None


def _generate_legal(fen: str) -> List[Move]:
    buf = (c_int32 * MAX_MOVES)()
    with _lock:
        n = _lib.chess_legal_moves(fen.encode("ascii"), buf, MAX_MOVES)
    if n < 0:
        raise InvalidFenError(f"invalid FEN: {fen!r}")
    if n > MAX_MOVES:
        raise EngineError(f"position has {n} legal moves; buffer holds {MAX_MOVES}")
    return [Move(buf[i]) for i in range(n)]


# ---------------------------------------------------------------------------
# Board
# ---------------------------------------------------------------------------

class Board:
    """
    A chess position with move making, legality, evaluation, perft and search.

    Draw rules: the fifty-move rule is detected; threefold repetition and
    insufficient material are not.
    """

    def __init__(self, fen: str = STARTING_FEN) -> None:
        parts = fen.split()
        if len(parts) < 4:
            raise InvalidFenError(f"invalid FEN: {fen!r}")
        while len(parts) < 6:  # tolerate FENs without the clock fields
            parts.append("0" if len(parts) == 4 else "1")
        self._fen = " ".join(parts)
        self._history: List[str] = []
        self._legal: Optional[List[Move]] = None
        self._get_legal()  # validates the FEN

    # ----- basic state -----

    @property
    def fen(self) -> str:
        return self._fen

    @property
    def turn(self) -> Color:
        return Color.WHITE if self._fen.split()[1] == "w" else Color.BLACK

    @property
    def fullmove_number(self) -> int:
        return int(self._fen.split()[5])

    @property
    def halfmove_clock(self) -> int:
        return int(self._fen.split()[4])

    def copy(self) -> "Board":
        new = Board(self._fen)
        new._history = list(self._history)
        return new

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Board) and other._fen == self._fen

    def __repr__(self) -> str:
        return f"Board({self._fen!r})"

    def __str__(self) -> str:
        rows = self._fen.split()[0].split("/")
        lines = []
        for i, row in enumerate(rows):
            cells: List[str] = []
            for ch in row:
                if ch.isdigit():
                    cells.extend(["."] * int(ch))
                else:
                    cells.append(ch)
            lines.append(f"{8 - i} " + " ".join(cells))
        lines.append("  a b c d e f g h")
        return "\n".join(lines)

    # ----- legality and making moves -----

    def _get_legal(self) -> List[Move]:
        if self._legal is None:
            self._legal = _generate_legal(self._fen)
        return self._legal

    def legal_moves(self) -> List[Move]:
        return list(self._get_legal())

    def is_legal(self, move: Union[Move, str]) -> bool:
        if isinstance(move, str):
            return any(m.uci == move.strip().lower() for m in self._get_legal())
        return move in self._get_legal()

    def parse_uci(self, uci: str) -> Move:
        uci = uci.strip().lower()
        for m in self._get_legal():
            if m.uci == uci:
                return m
        raise IllegalMoveError(f"{uci!r} is not legal in {self._fen!r}")

    def push(self, move: Union[Move, str]) -> Move:
        """Play a legal move (a Move or a UCI string such as 'e2e4')."""
        if isinstance(move, str):
            move = self.parse_uci(move)
        if not isinstance(move, Move):
            raise TypeError(f"expected Move or str, got {type(move).__name__}")
        if move not in self._get_legal():
            raise IllegalMoveError(f"{move.uci!r} is not legal in {self._fen!r}")

        out = create_string_buffer(_FEN_BUF)
        with _lock:
            rc = _lib.chess_make_move(self._fen.encode("ascii"), move.code,
                                      out, _FEN_BUF)
        if rc != 0:
            raise EngineError(f"chess_make_move failed with code {rc}")

        self._history.append(self._fen)
        self._fen = out.value.decode("ascii")
        self._legal = None
        return move

    def push_uci(self, uci: str) -> Move:
        return self.push(uci)

    def pop(self) -> None:
        """Undo the last move made with push()."""
        if not self._history:
            raise IndexError("no moves to undo")
        self._fen = self._history.pop()
        self._legal = None

    # ----- game state -----

    def in_check(self) -> bool:
        with _lock:
            rc = _lib.chess_in_check(self._fen.encode("ascii"))
        if rc < 0:
            raise InvalidFenError(f"invalid position: {self._fen!r}")
        return rc == 1

    def is_checkmate(self) -> bool:
        return not self._get_legal() and self.in_check()

    def is_stalemate(self) -> bool:
        return not self._get_legal() and not self.in_check()

    def is_game_over(self) -> bool:
        return not self._get_legal() or self.halfmove_clock >= 100

    def result(self) -> Optional[str]:
        """'1-0', '0-1', '1/2-1/2', or None if the game is still in progress."""
        if self._get_legal():
            return "1/2-1/2" if self.halfmove_clock >= 100 else None
        if self.in_check():
            return "0-1" if self.turn == Color.WHITE else "1-0"
        return "1/2-1/2"

    # ----- analysis -----

    def evaluate(self) -> int:
        """Static evaluation in centipawns from the side to move's perspective."""
        score = c_int32(0)
        with _lock:
            rc = _lib.chess_eval(self._fen.encode("ascii"), byref(score))
        if rc != 0:
            raise InvalidFenError(f"invalid FEN: {self._fen!r}")
        return score.value

    def perft(self, depth: int) -> PerftResult:
        """Count leaf nodes and statistics to the given depth (in plies)."""
        counts = (c_int64 * 6)()
        with _lock:
            rc = _lib.chess_perft(self._fen.encode("ascii"), depth, counts)
        if rc != 0:
            raise InvalidFenError(f"invalid FEN: {self._fen!r}")
        return PerftResult(*counts)

    def search(self, depth: int = 4, *, clear_tt: bool = False) -> SearchResult:
        """
        Iterative-deepening alpha-beta search to a fixed depth.
        Set clear_tt=True to discard cached transposition-table entries first.
        """
        if depth < 1:
            raise ValueError("depth must be >= 1")
        if clear_tt:
            clear_transposition_table()

        best = c_int32(0)
        score = c_int32(0)
        nodes = c_int64(0)
        with _lock:
            rc = _lib.chess_search(self._fen.encode("ascii"), depth,
                                   byref(best), byref(score), byref(nodes))
        if rc != 0:
            raise InvalidFenError(f"invalid FEN: {self._fen!r}")

        if best.value == 0:  # no legal moves: game is over
            final_score = -MATE_SCORE if self.in_check() else 0
            return SearchResult(None, final_score, nodes.value, depth)

        return SearchResult(Move(best.value), score.value, nodes.value, depth)


# ---------------------------------------------------------------------------
# Example: engine self-play
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    board = Board()
    print(board, "\n")
    for _ in range(200):
        if board.result() is not None:
            break
        result = board.search(depth=3)
        board.push(result.best_move)
    print(board, "\n")
    print("Result:", board.result() or "in progress")
    print("FEN:   ", board.fen)