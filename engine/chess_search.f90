! =============================================================================
!  chess_search.f90
!  Evaluation, Transposition Table, and Alpha-Beta Search (Fortran 2008)
!
!  Features
!    * Static evaluation combining piece values (P=100, N=300, B=300, R=500,
!      Q=1000) and standard piece-square positional tables (PST).
!    * Zobrist-hashed transposition table (TT) with mate-score ply adjustment.
!    * Move ordering: TT hash move first, then MVV-LVA + promotion bonuses.
!    * Negamax alpha-beta search with iterative deepening and quiescence search.
!    * ISO_C_BINDING interface for shared library (.so) interoperability.
! =============================================================================
module chess_search_mod
  use, intrinsic :: iso_fortran_env, only: int32, int64
  use, intrinsic :: iso_c_binding
  use chess_engine
  implicit none
  private

  public :: search_info_t
  public :: init_search, clear_tt, evaluate, search_position
  public :: chess_search_c, chess_eval_c, chess_clear_tt_c
  public :: MATE_SCORE, INF_SCORE

  integer, parameter :: INF_SCORE  = 30000
  integer, parameter :: MATE_SCORE = 29000
  integer, parameter :: MATE_THRESHOLD = 28000
  integer, parameter :: MAX_PLY    = 128

  ! Piece values in centipawns: P=100, N=300, B=300, R=500, Q=1000, K=0
  integer, parameter :: PIECE_VAL(6) = [100, 300, 300, 500, 1000, 0]

  ! Transposition table bound types
  integer, parameter :: TT_NONE  = 0
  integer, parameter :: TT_EXACT = 1
  integer, parameter :: TT_ALPHA = 2  ! upper bound (failed low)
  integer, parameter :: TT_BETA  = 3  ! lower bound (failed high)

  ! Transposition table size: 2^20 entries (~24 MB)
  integer, parameter :: TT_BITS = 20
  integer, parameter :: TT_SIZE = ishft(1, TT_BITS)
  integer(int64), parameter :: TT_MASK = int(TT_SIZE - 1, int64)

  type :: tt_entry_t
    integer(int64) :: key   = 0_int64
    integer(int32) :: move  = 0_int32
    integer(int32) :: score = 0_int32
    integer(int32) :: depth = -1_int32
    integer(int32) :: flag  = TT_NONE
  end type tt_entry_t

  type :: search_info_t
    integer(int32) :: best_move = 0_int32
    integer(int32) :: score     = 0_int32
    integer(int64) :: nodes     = 0_int64
    integer        :: depth     = 0
  end type search_info_t

  ! Pre-combined material + positional table: EVAL_TABLE(piece_idx 1..12, sq 0..63)
  logical, save :: search_initialised = .false.
  integer, save :: EVAL_TABLE(12, 0:63)
  type(tt_entry_t), save :: TT(0:TT_SIZE - 1)

  ! ---------------------------------------------------------------------------
  !  Piece-Square Tables from White's perspective (rank 8 down to rank 1,
  !  files a..h on each row). Visual top row is a8..h8, bottom row is a1..h1.
  ! ---------------------------------------------------------------------------
  integer, parameter :: PST_PAWN(0:63) = [ &
     0,   0,   0,   0,   0,   0,   0,   0, &
    50,  50,  50,  50,  50,  50,  50,  50, &
    10,  10,  20,  30,  30,  20,  10,  10, &
     5,   5,  10,  25,  25,  10,   5,   5, &
     0,   0,   0,  20,  20,   0,   0,   0, &
     5,  -5, -10,   0,   0, -10,  -5,   5, &
     5,  10,  10, -20, -20,  10,  10,   5, &
     0,   0,   0,   0,   0,   0,   0,   0  ]

  integer, parameter :: PST_KNIGHT(0:63) = [ &
   -50, -40, -30, -30, -30, -30, -40, -50, &
   -40, -20,   0,   0,   0,   0, -20, -40, &
   -30,   0,  10,  15,  15,  10,   0, -30, &
   -30,   5,  15,  20,  20,  15,   5, -30, &
   -30,   0,  15,  20,  20,  15,   0, -30, &
   -30,   5,  10,  15,  15,  10,   5, -30, &
   -40, -20,   0,   5,   5,   0, -20, -40, &
   -50, -40, -30, -30, -30, -30, -40, -50  ]

  integer, parameter :: PST_BISHOP(0:63) = [ &
   -20, -10, -10, -10, -10, -10, -10, -20, &
   -10,   0,   0,   0,   0,   0,   0, -10, &
   -10,   0,   5,  10,  10,   5,   0, -10, &
   -10,   5,   5,  10,  10,   5,   5, -10, &
   -10,   0,  10,  10,  10,  10,   0, -10, &
   -10,  10,  10,  10,  10,  10,  10, -10, &
   -10,   5,   0,   0,   0,   0,   5, -10, &
   -20, -10, -10, -10, -10, -10, -10, -20  ]

  integer, parameter :: PST_ROOK(0:63) = [ &
     0,   0,   0,   0,   0,   0,   0,   0, &
     5,  10,  10,  10,  10,  10,  10,   5, &
    -5,   0,   0,   0,   0,   0,   0,  -5, &
    -5,   0,   0,   0,   0,   0,   0,  -5, &
    -5,   0,   0,   0,   0,   0,   0,  -5, &
    -5,   0,   0,   0,   0,   0,   0,  -5, &
    -5,   0,   0,   0,   0,   0,   0,  -5, &
     0,   0,   0,   5,   5,   0,   0,   0  ]

  integer, parameter :: PST_QUEEN(0:63) = [ &
   -20, -10, -10,  -5,  -5, -10, -10, -20, &
   -10,   0,   0,   0,   0,   0,   0, -10, &
   -10,   0,   5,   5,   5,   5,   0, -10, &
    -5,   0,   5,   5,   5,   5,   0,  -5, &
     0,   0,   5,   5,   5,   5,   0,  -5, &
   -10,   5,   5,   5,   5,   5,   0, -10, &
   -10,   0,   5,   0,   0,   0,   0, -10, &
   -20, -10, -10,  -5,  -5, -10, -10, -20  ]

  integer, parameter :: PST_KING(0:63) = [ &
   -30, -40, -40, -50, -50, -40, -40, -30, &
   -30, -40, -40, -50, -50, -40, -40, -30, &
   -30, -40, -40, -50, -50, -40, -40, -30, &
   -30, -40, -40, -50, -50, -40, -40, -30, &
   -20, -30, -30, -40, -40, -30, -30, -20, &
   -10, -20, -20, -20, -20, -20, -20, -10, &
    20,  20,   0,   0,   0,   0,  20,  20, &
    20,  30,  10,   0,   0,  10,  30,  20  ]

contains

  ! ---------------------------------------------------------------------------
  !  Initialise evaluation tables and transposition table
  ! ---------------------------------------------------------------------------
  subroutine init_search()
    integer :: sq, w_idx, b_idx

    if (search_initialised) return
    call init_engine()

    do sq = 0, 63
      ! Map engine square (a1=0..h8=63) to visual PST index (a8=0..h1=63)
      w_idx = ieor(sq, 56)
      ! Black mirrors vertically relative to White (a1=0..h8=63)
      b_idx = sq

      EVAL_TABLE(PAWN,   sq) = PIECE_VAL(PAWN)   + PST_PAWN(w_idx)
      EVAL_TABLE(KNIGHT, sq) = PIECE_VAL(KNIGHT) + PST_KNIGHT(w_idx)
      EVAL_TABLE(BISHOP, sq) = PIECE_VAL(BISHOP) + PST_BISHOP(w_idx)
      EVAL_TABLE(ROOK,   sq) = PIECE_VAL(ROOK)   + PST_ROOK(w_idx)
      EVAL_TABLE(QUEEN,  sq) = PIECE_VAL(QUEEN)  + PST_QUEEN(w_idx)
      EVAL_TABLE(KING,   sq) = PIECE_VAL(KING)   + PST_KING(w_idx)

      EVAL_TABLE(6 + PAWN,   sq) = PIECE_VAL(PAWN)   + PST_PAWN(b_idx)
      EVAL_TABLE(6 + KNIGHT, sq) = PIECE_VAL(KNIGHT) + PST_KNIGHT(b_idx)
      EVAL_TABLE(6 + BISHOP, sq) = PIECE_VAL(BISHOP) + PST_BISHOP(b_idx)
      EVAL_TABLE(6 + ROOK,   sq) = PIECE_VAL(ROOK)   + PST_ROOK(b_idx)
      EVAL_TABLE(6 + QUEEN,  sq) = PIECE_VAL(QUEEN)  + PST_QUEEN(b_idx)
      EVAL_TABLE(6 + KING,   sq) = PIECE_VAL(KING)   + PST_KING(b_idx)
    end do

    call clear_tt()
    search_initialised = .true.
  end subroutine init_search

  subroutine clear_tt()
    TT%key   = 0_int64
    TT%move  = 0_int32
    TT%score = 0_int32
    TT%depth = -1_int32
    TT%flag  = TT_NONE
  end subroutine clear_tt

  ! ---------------------------------------------------------------------------
  !  Static Evaluation (returns score in centipawns from p%side's perspective)
  ! ---------------------------------------------------------------------------
  pure integer function evaluate(p) result(score)
    type(position_t), intent(in) :: p
    integer :: pc, sq, w_score, b_score
    integer(int64) :: b

    w_score = 0
    do pc = 1, 6
      b = p%bb(pc)
      do while (b /= 0_int64)
        sq = trailz(b)
        b = iand(b, b - 1_int64)
        w_score = w_score + EVAL_TABLE(pc, sq)
      end do
    end do

    b_score = 0
    do pc = 7, 12
      b = p%bb(pc)
      do while (b /= 0_int64)
        sq = trailz(b)
        b = iand(b, b - 1_int64)
        b_score = b_score + EVAL_TABLE(pc, sq)
      end do
    end do

    if (p%side == WHITE) then
      score = w_score - b_score
    else
      score = b_score - w_score
    end if
  end function evaluate

  ! ---------------------------------------------------------------------------
  !  Transposition Table Probe and Store
  ! ---------------------------------------------------------------------------
  subroutine tt_probe(key, depth, ply, alpha, beta, hit, score, tt_move)
    integer(int64), intent(in) :: key
    integer, intent(in) :: depth, ply, alpha, beta
    logical, intent(out) :: hit
    integer, intent(out) :: score
    integer(int32), intent(out) :: tt_move
    integer(int64) :: idx
    integer :: s

    hit = .false.
    score = 0
    tt_move = 0_int32
    idx = iand(key, TT_MASK)

    if (TT(idx)%flag /= TT_NONE .and. TT(idx)%key == key) then
      tt_move = TT(idx)%move
      if (TT(idx)%depth >= depth) then
        s = int(TT(idx)%score)
        if (s > MATE_THRESHOLD) s = s - ply
        if (s < -MATE_THRESHOLD) s = s + ply

        if (TT(idx)%flag == TT_EXACT) then
          score = s
          hit = .true.
        else if (TT(idx)%flag == TT_ALPHA .and. s <= alpha) then
          score = alpha
          hit = .true.
        else if (TT(idx)%flag == TT_BETA .and. s >= beta) then
          score = beta
          hit = .true.
        end if
      end if
    end if
  end subroutine tt_probe

  subroutine tt_store(key, depth, ply, score, flag, move)
    integer(int64), intent(in) :: key
    integer, intent(in) :: depth, ply, score, flag
    integer(int32), intent(in) :: move
    integer(int64) :: idx
    integer :: s

    idx = iand(key, TT_MASK)
    s = score
    if (s > MATE_THRESHOLD) s = s + ply
    if (s < -MATE_THRESHOLD) s = s - ply

    TT(idx)%key   = key
    if (move /= 0_int32 .or. TT(idx)%key /= key) TT(idx)%move = move
    TT(idx)%score = int(s, int32)
    TT(idx)%depth = int(depth, int32)
    TT(idx)%flag  = int(flag, int32)
  end subroutine tt_store

  ! ---------------------------------------------------------------------------
  !  Move Ordering Helpers (TT move first + MVV-LVA + Promotions)
  ! ---------------------------------------------------------------------------
  pure integer function piece_type_at(p, side, sq) result(pt)
    type(position_t), intent(in) :: p
    integer, intent(in) :: side, sq
    integer :: o, k
    integer(int64) :: b
    o = 6*side
    b = sqbit(sq)
    pt = 0
    do k = 1, 6
      if (iand(p%bb(o + k), b) /= 0_int64) then
        pt = k
        return
      end if
    end do
  end function piece_type_at

  subroutine score_moves(p, ml, tt_move, scores)
    type(position_t), intent(in) :: p
    type(move_list_t), intent(in) :: ml
    integer(int32), intent(in) :: tt_move
    integer, intent(out) :: scores(:)
    integer :: i, m, from, to, promo, attacker, victim

    do i = 1, ml%n
      m = ml%m(i)
      if (m == tt_move .and. tt_move /= 0_int32) then
        scores(i) = 1000000
      else
        scores(i) = 0
        from  = iand(m, 63)
        to    = iand(ishft(m, -6), 63)
        promo = iand(ishft(m, -12), 7)

        if (iand(m, MF_CAPTURE) /= 0) then
          attacker = piece_type_at(p, p%side, from)
          if (iand(m, MF_EP) /= 0) then
            victim = PAWN
          else
            victim = piece_type_at(p, 1 - p%side, to)
          end if
          if (victim > 0 .and. attacker > 0) then
            scores(i) = 100000 + 10 * PIECE_VAL(victim) - PIECE_VAL(attacker) / 100
          else
            scores(i) = 100000
          end if
        end if

        if (promo /= 0) then
          scores(i) = scores(i) + 50000 + PIECE_VAL(promo)
        end if
      end if
    end do
  end subroutine score_moves

  ! Selection-sort step: brings the highest-scoring remaining move to index 'idx'
  subroutine pick_move(ml, scores, idx)
    type(move_list_t), intent(inout) :: ml
    integer, intent(inout) :: scores(:)
    integer, intent(in) :: idx
    integer :: j, best_idx, best_s
    integer(int32) :: tmp_m

    best_idx = idx
    best_s = scores(idx)
    do j = idx + 1, ml%n
      if (scores(j) > best_s) then
        best_s = scores(j)
        best_idx = j
      end if
    end do

    if (best_idx /= idx) then
      tmp_m = ml%m(idx)
      ml%m(idx) = ml%m(best_idx)
      ml%m(best_idx) = tmp_m

      scores(best_idx) = scores(idx)
      scores(idx) = best_s
    end if
  end subroutine pick_move

  ! ---------------------------------------------------------------------------
  !  Quiescence Search (resolves tactical captures/promotions at leaf nodes)
  ! ---------------------------------------------------------------------------
  recursive integer function quiescence(p, alpha_in, beta, ply, nodes) result(best_score)
    type(position_t), intent(in) :: p
    integer, intent(in) :: alpha_in, beta, ply
    integer(int64), intent(inout) :: nodes
    type(move_list_t) :: pl, ml
    type(position_t) :: np
    integer :: scores(512)
    integer :: alpha, stand_pat, score, i, legal_moves
    integer(int32) :: m
    logical :: in_check

    nodes = nodes + 1_int64
    alpha = alpha_in

    if (ply >= MAX_PLY) then
      best_score = evaluate(p)
      return
    end if

    in_check = is_attacked(p, king_square(p, p%side), 1 - p%side)

    if (.not. in_check) then
      stand_pat = evaluate(p)
      if (stand_pat >= beta) then
        best_score = beta
        return
      end if
      if (stand_pat > alpha) alpha = stand_pat
      best_score = stand_pat
    else
      best_score = -INF_SCORE
    end if

    pl%n = 0
    call gen_pseudo(p, pl)

    ! Filter moves: if in check, try all evasions; otherwise only captures & promotions
    ml%n = 0
    do i = 1, pl%n
      m = pl%m(i)
      if (in_check .or. iand(m, MF_CAPTURE) /= 0 .or. iand(m, MF_PROMO_MASK) /= 0) then
        ml%n = ml%n + 1
        ml%m(ml%n) = m
      end if
    end do

    call score_moves(p, ml, 0_int32, scores)

    legal_moves = 0
    do i = 1, ml%n
      call pick_move(ml, scores, i)
      m = ml%m(i)
      np = make_move(p, m)
      if (is_attacked(np, king_square(np, p%side), np%side)) cycle
      legal_moves = legal_moves + 1

      score = -quiescence(np, -beta, -alpha, ply + 1, nodes)

      if (score >= beta) then
        best_score = beta
        return
      end if
      if (score > best_score) best_score = score
      if (score > alpha) alpha = score
    end do

    if (in_check .and. legal_moves == 0) then
      best_score = -MATE_SCORE + ply
    end if
  end function quiescence

  ! ---------------------------------------------------------------------------
  !  Negamax Alpha-Beta Search
  ! ---------------------------------------------------------------------------
  recursive integer function negamax(p, depth, alpha_in, beta, ply, nodes, best_move_out) result(best_score)
    type(position_t), intent(in) :: p
    integer, intent(in) :: depth, alpha_in, beta, ply
    integer(int64), intent(inout) :: nodes
    integer(int32), intent(out) :: best_move_out
    type(move_list_t) :: pl
    type(position_t) :: np
    integer :: scores(512)
    integer :: alpha, score, i, legal_moves, tt_flag
    integer(int32) :: tt_move, m
    logical :: tt_hit, in_check

    best_move_out = 0_int32

    ! Fifty-move rule draw check (above root)
    if (ply > 0 .and. p%halfmove >= 100) then
      best_score = 0
      return
    end if

    ! Probe transposition table
    call tt_probe(p%hash, depth, ply, alpha_in, beta, tt_hit, score, tt_move)
    if (ply > 0 .and. tt_hit) then
      best_score = score
      best_move_out = tt_move
      return
    end if

    ! Base case: drop into quiescence search
    if (depth <= 0 .or. ply >= MAX_PLY) then
      best_score = quiescence(p, alpha_in, beta, ply, nodes)
      return
    end if

    nodes = nodes + 1_int64
    alpha = alpha_in
    best_score = -INF_SCORE
    legal_moves = 0

    pl%n = 0
    call gen_pseudo(p, pl)
    call score_moves(p, pl, tt_move, scores)

    do i = 1, pl%n
      call pick_move(pl, scores, i)
      m = pl%m(i)
      np = make_move(p, m)
      if (is_attacked(np, king_square(np, p%side), np%side)) cycle
      legal_moves = legal_moves + 1

      if (best_move_out == 0_int32) best_move_out = m

      score = -negamax(np, depth - 1, -beta, -alpha, ply + 1, nodes, m)

      if (score > best_score) then
        best_score = score
        best_move_out = pl%m(i)
      end if

      if (score > alpha) alpha = score
      if (alpha >= beta) exit
    end do

    ! Checkmate or stalemate detection
    if (legal_moves == 0) then
      in_check = is_attacked(p, king_square(p, p%side), 1 - p%side)
      if (in_check) then
        best_score = -MATE_SCORE + ply
      else
        best_score = 0
      end if
      return
    end if

    ! Store result in transposition table
    if (best_score <= alpha_in) then
      tt_flag = TT_ALPHA
    else if (best_score >= beta) then
      tt_flag = TT_BETA
    else
      tt_flag = TT_EXACT
    end if
    call tt_store(p%hash, depth, ply, best_score, tt_flag, best_move_out)
  end function negamax

  ! ---------------------------------------------------------------------------
  !  Top-level iterative deepening search driver
  ! ---------------------------------------------------------------------------
  subroutine search_position(p, max_depth, info)
    type(position_t), intent(in) :: p
    integer, intent(in) :: max_depth
    type(search_info_t), intent(out) :: info
    type(move_list_t) :: ml
    integer :: d, target_depth, score
    integer(int32) :: iter_move

    call init_search()

    info%best_move = 0_int32
    info%score     = 0_int32
    info%nodes     = 0_int64
    target_depth   = max(1, max_depth)
    info%depth     = target_depth

    ! Fallback legal move in case search terminates early
    call gen_legal(p, ml)
    if (ml%n == 0) return
    info%best_move = ml%m(1)

    ! Iterative deepening populates the TT and refines move ordering
    do d = 1, target_depth
      score = negamax(p, d, -INF_SCORE, INF_SCORE, 0, info%nodes, iter_move)
      if (iter_move /= 0_int32) info%best_move = iter_move
      info%score = int(score, int32)
    end do
  end subroutine search_position

  ! ---------------------------------------------------------------------------
  !  C API bindings for shared library (.so) usage
  ! ---------------------------------------------------------------------------

  ! Clear the transposition table
  subroutine chess_clear_tt_c() bind(C, name="chess_clear_tt")
    call init_search()
    call clear_tt()
  end subroutine chess_clear_tt_c

  ! Static evaluation of a FEN string from the side-to-move's perspective.
  ! Returns 0 on success, -1 on invalid FEN.
  integer(c_int) function chess_eval_c(fen, score) bind(C, name="chess_eval")
    character(kind=c_char), dimension(*), intent(in) :: fen
    integer(c_int32_t), intent(out) :: score
    type(position_t) :: p
    logical :: ok

    call init_search()
    score = 0_c_int32_t
    call parse_fen(c_to_f(fen), p, ok)
    if (.not. ok) then
      chess_eval_c = -1_c_int
      return
    end if
    score = int(evaluate(p), c_int32_t)
    chess_eval_c = 0_c_int
  end function chess_eval_c

  ! Search a FEN position up to fixed depth.
  ! Outputs: best_move (encoded int32, 0 if no legal moves), score (centipawns
  ! from side to move's perspective), and nodes visited.
  ! Returns 0 on success, -1 on invalid FEN.
  integer(c_int) function chess_search_c(fen, depth, best_move, score, nodes) bind(C, name="chess_search")
    character(kind=c_char), dimension(*), intent(in) :: fen
    integer(c_int), value, intent(in) :: depth
    integer(c_int32_t), intent(out) :: best_move
    integer(c_int32_t), intent(out) :: score
    integer(c_int64_t), intent(out) :: nodes
    type(position_t) :: p
    type(search_info_t) :: info
    logical :: ok

    best_move = 0_c_int32_t
    score     = 0_c_int32_t
    nodes     = 0_c_int64_t

    call init_search()
    call parse_fen(c_to_f(fen), p, ok)
    if (.not. ok) then
      chess_search_c = -1_c_int
      return
    end if

    call search_position(p, int(depth), info)
    best_move = info%best_move
    score     = info%score
    nodes     = info%nodes
    chess_search_c = 0_c_int
  end function chess_search_c

end module chess_search_mod