import random
import engine.chess_binding as chess
from tqdm import tqdm

n_steps = 100

board = chess.Board()
for _ in tqdm(range(n_steps)):
    moves = list(board.legal_moves())
    if len(moves) == 0:
        break
    move = random.choice(moves)
    board.push(move)

print(board.fen)