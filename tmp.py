import engine.chess_binding as chess
import time

for depth in range(1, 10):
    board = chess.Board()
    t_start = time.time()
    _ = board.search(depth, clear_tt=True)
    t_end = time.time()
    print(f"Depth: {depth}, Time: {t_end - t_start:.4f} seconds")

    