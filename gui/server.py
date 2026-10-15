import os
import threading
from ctypes import (CDLL, POINTER, c_char, c_char_p, c_int, c_int32, c_int64,
                    create_string_buffer)

from fastapi import FastAPI, HTTPException, Query
from fastapi.responses import FileResponse, JSONResponse

HERE = os.path.dirname(os.path.abspath(__file__))
STATIC_DIR = os.path.realpath(os.path.join(HERE, "static"))
LIB_PATH = os.path.join(HERE, "..", "engine", "bin", "libchess.so")

SEARCH_DEPTH = 7
MAX_MOVES = 512           # matches move_list_t capacity in the engine
MATE_THRESHOLD = 28000    # mirrors chess_search.f90
MATE_SCORE = 29000

if not os.path.exists(LIB_PATH):
    raise RuntimeError(f"{LIB_PATH} not found; run `sh engine/build.sh` first")

lib = CDLL(LIB_PATH)

# Declare signatures so ctypes marshals arguments correctly.
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

lib.chess_move_uci.argtypes = [c_int32, POINTER(c_char)]
lib.chess_move_uci.restype = None

lib.chess_init()

# The engine keeps global state (transposition table, lookup tables), so
# every call into it is serialised. FastAPI runs `def` endpoints in a thread
# pool, so this lock is what keeps concurrent requests from corrupting it.
_engine_lock = threading.Lock()

app = FastAPI()


def _normalise_fen(fen: str) -> str:
    # Accept "." as a stand-in for "/" (the convention this server already used)
    fen = " ".join(fen.replace(".", "/").split())
    if not fen:
        raise HTTPException(status_code=400, detail="FEN is empty")
    try:
        fen.encode("ascii")
    except UnicodeEncodeError:
        raise HTTPException(status_code=400, detail="FEN must be ASCII")
    return fen


def _invalid_fen(fen: str) -> HTTPException:
    return HTTPException(status_code=400, detail=f"invalid FEN: {fen}")


def _side_to_move(fen: str) -> str:
    parts = fen.split()
    return parts[1] if len(parts) > 1 else "w"


def _uci(move: int) -> str:
    buf = create_string_buffer(8)
    lib.chess_move_uci(move, buf)
    return buf.value.decode("ascii")


def _white_perspective(score_stm: int, fen: str) -> int:
    # Engine scores are from the side to move; convert to White's view.
    return score_stm if _side_to_move(fen) == "w" else -score_stm


def _mate_in(score_stm: int) -> int | None:
    """Return moves-to-mate from the side to move (positive = side to move mates)."""
    if abs(score_stm) <= MATE_THRESHOLD:
        return None
    plies = MATE_SCORE - abs(score_stm)
    moves = (plies + 1) // 2
    return moves if score_stm > 0 else -moves


@app.get("/api/v1/legal_moves")
def get_legal_moves(fen: str):
    fen = _normalise_fen(fen)
    moves = (c_int32 * MAX_MOVES)()
    with _engine_lock:
        n = lib.chess_legal_moves(fen.encode("ascii"), moves, MAX_MOVES)
    if n < 0:
        raise _invalid_fen(fen)
    uci = [_uci(moves[i]) for i in range(min(n, MAX_MOVES))]
    return {"legal_moves": uci, "count": n}


@app.get("/api/v1/evaluate")
def evaluate_position(fen: str):
    fen = _normalise_fen(fen)
    score = c_int32(0)
    with _engine_lock:
        rc = lib.chess_eval(fen.encode("ascii"), score)
    if rc != 0:
        raise _invalid_fen(fen)
    cp = _white_perspective(score.value, fen)
    return {"evaluation": cp / 100.0, "evaluation_cp": cp}


@app.get("/api/v1/best_move")
def best_move(
    fen: str,
    depth: int = Query(SEARCH_DEPTH, ge=1, le=12),
):
    fen = _normalise_fen(fen)
    move = c_int32(0)
    score = c_int32(0)
    nodes = c_int64(0)
    with _engine_lock:
        rc = lib.chess_search(fen.encode("ascii"), depth,
                              move, score, nodes)
    if rc != 0:
        raise _invalid_fen(fen)

    stm_score = score.value
    return {
        "best_move": _uci(move.value) if move.value != 0 else None,
        "depth": depth,
        "score_cp": _white_perspective(stm_score, fen),
        "mate_in": _mate_in(stm_score),
        "nodes": nodes.value,
    }


@app.get("/api/v1/perft")
def perft(
    fen: str,
    plies: int = Query(..., ge=1, le=6),
):
    fen = _normalise_fen(fen)
    counts = (c_int64 * 6)()
    with _engine_lock:
        rc = lib.chess_perft(fen.encode("ascii"), plies, counts)
    if rc != 0:
        raise _invalid_fen(fen)
    keys = ["nodes", "captures", "en_passant", "castles", "promotions", "checks"]
    return {"plies": plies, **dict(zip(keys, (int(c) for c in counts)))}


@app.get("/{path:path}")
async def serve_static(path: str):
    if path in ("", "/"):
        path = "index.html"
    full = os.path.realpath(os.path.join(STATIC_DIR, path))
    # Block path traversal outside the static directory
    if not full.startswith(STATIC_DIR + os.sep) or not os.path.isfile(full):
        return JSONResponse(content={"error": "File not found"}, status_code=404)
    return FileResponse(full)