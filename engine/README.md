# Fortran 2008 bitboard chess engine

Files
- `chess_engine.f90`  core module (bitboards, Zobrist hashing, move generation, make/perft, C API)
- `chess_search.f90`  evaluation (material + PST), transposition table, alpha-beta + quiescence search, C API
- `perft_main.f90`    command-line move generation verification driver
- `search_main.f90`   command-line best-move search driver
- `build.sh`          builds `libchess.so`, `./perft`, and `./search`

Build (gfortran >= 9):  `sh build.sh`   (or `FC=ifx sh build.sh`)

Verification (FEN + number of half-moves):
  `./perft "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 5`
  Nodes 4865609, Captures 82719, En passant 258, Castles 0, Promotions 0, Checks 27351

Search (FEN + depth):
  `./search "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 6`

C API (`libchess.so`)
```c
void    chess_init(void);
void    chess_clear_tt(void);
int     chess_perft(const char *fen, int plies, int64_t counts[6]);
        // counts = nodes, captures, en_passant, castles, promotions, checks
        // returns 0 on success, -1 on invalid FEN
int     chess_legal_moves(const char *fen, int32_t *moves, int max_moves);
        // returns number of legal moves (or -1 on invalid FEN)
int     chess_eval(const char *fen, int32_t *score);
        // static evaluation in centipawns from side-to-move's perspective
        // returns 0 on success, -1 on invalid FEN
int     chess_search(const char *fen, int depth, int32_t *best_move, int32_t *score, int64_t *nodes);
        // searches to fixed depth with iterative deepening, alpha-beta, TT, and quiescence
        // returns 0 on success, -1 on invalid FEN
void    chess_move_uci(int32_t move, char buf[>=6]);
```

Move encoding (int32): bits 0-5 from, 6-11 to, 12-14 promotion piece
(2=N 3=B 4=R 5=Q), 15 capture, 16 en passant, 17 castle.
