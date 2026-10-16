import engine.chess_binding as chess
import time
import sys

fen_str = None
max_depth = 18

if len(sys.argv) == 2:
    fen_str = sys.argv[1]
elif len(sys.argv) == 3:
    fen_str = sys.argv[1]
    max_depth = int(sys.argv[2])

if fen_str is None or fen_str == "":
    fen_str = chess.STARTING_FEN

for depth in range(2, max_depth + 1, 2):
    board = chess.Board(fen=fen_str)
    t_start = time.time()
    res = board.search(depth, clear_tt=True)
    t_end = time.time()
    print(f"Depth: {depth:2d}, Time: {t_end - t_start:.4f}s, Nodes: {res.nodes:,}, Score: {res.score}, best move: {res.best_move}")

    