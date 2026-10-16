import os
import sys
from dataclasses import asdict
import time

from fastapi import FastAPI, HTTPException, Query
from fastapi.responses import FileResponse, JSONResponse

HERE = os.path.dirname(os.path.abspath(__file__))
ENGINE_DIR = os.path.realpath(os.path.join(HERE, "..", "engine"))
STATIC_DIR = os.path.realpath(os.path.join(HERE, "static"))

if ENGINE_DIR not in sys.path:
    sys.path.insert(0, ENGINE_DIR)

try:
    from chess_binding import Board, Color, EngineError, InvalidFenError
except FileNotFoundError as exc:
    raise RuntimeError(
        f"{exc}. Run `sh engine/build.sh` first, or set FCHESS_LIB."
    ) from exc

SEARCH_DEPTH = 16

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


def _load_board(fen: str) -> Board:
    """Normalise the FEN and build a Board, turning parse errors into HTTP 400s."""
    fen = _normalise_fen(fen)
    try:
        return Board(fen)
    except (InvalidFenError, ValueError):
        raise _invalid_fen(fen)


def _white_perspective(score_stm: int, board: Board) -> int:
    # The binding reports scores from the side to move; convert to White's view.
    return score_stm if board.turn == Color.WHITE else -score_stm


@app.get("/api/v1/legal_moves")
def get_legal_moves(fen: str):
    t0 = time.time()
    board = _load_board(fen)
    try:
        moves = [m.uci for m in board.legal_moves()]
    except (InvalidFenError, EngineError):
        raise _invalid_fen(fen)
    return {"legal_moves": moves, "count": len(moves), "time": time.time() - t0}


@app.get("/api/v1/evaluate")
def evaluate_position(fen: str):
    t0 = time.time()
    board = _load_board(fen)
    try:
        cp = _white_perspective(board.evaluate(), board)
    except (InvalidFenError, EngineError):
        raise _invalid_fen(fen)
    return {"evaluation": cp / 100.0, "evaluation_cp": cp, "time": time.time() - t0}


@app.get("/api/v1/best_move")
def best_move(
    fen: str,
    depth: int = Query(SEARCH_DEPTH, ge=1, le=20),
):
    t0 = time.time()
    board = _load_board(fen)
    try:
        result = board.search(depth)
    except (InvalidFenError, EngineError):
        raise _invalid_fen(fen)

    return {
        "best_move": result.best_move.uci if result.best_move else None,
        "depth": depth,
        "score_cp": _white_perspective(result.score, board),
        "mate_in": result.mate_in,  # from the side to move's perspective
        "nodes": result.nodes,
        "time": time.time() - t0,
    }


@app.get("/api/v1/perft")
def perft(
    fen: str,
    plies: int = Query(..., ge=1, le=6),
):
    t0 = time.time()
    board = _load_board(fen)
    try:
        counts = board.perft(plies)
    except (InvalidFenError, EngineError):
        raise _invalid_fen(fen)
    return {"plies": plies, **asdict(counts), "time": time.time() - t0}


@app.get("/{path:path}")
async def serve_static(path: str):
    if path in ("", "/"):
        path = "index.html"
    full = os.path.realpath(os.path.join(STATIC_DIR, path))
    # Block path traversal outside the static directory
    if not full.startswith(STATIC_DIR + os.sep) or not os.path.isfile(full):
        return JSONResponse(content={"error": "File not found"}, status_code=404)
    return FileResponse(full)