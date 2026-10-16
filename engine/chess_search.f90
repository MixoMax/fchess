! =============================================================================
!  chess_search.f90
!  Search for the Fortran chess engine (Fortran 2008 + OpenMP)
!
!  Features
!    * Negamax principal variation search (PVS) with iterative deepening
!      and aspiration windows at the root.
!    * Lock-free, shared transposition table (Lazy SMP). Each entry is stored
!      as two int64 words: key = hash XOR data, data = packed entry. A torn
!      read from a concurrent write fails the check and is treated as a miss.
!    * Null-move pruning, late move reductions, razoring, futility pruning,
!      check extensions, mate-distance pruning.
!    * Quiescence search with delta pruning.
!    * Move ordering: TT move, MVV-LVA captures, promotions, killers, history.
!    * Incremental static evaluation (position_t%score).
!
!  Lazy SMP: every thread runs iterative deepening on the same root. Helper
!  threads skip some iteration depths and perturb root move ordering so their
!  TT writes differ. The main thread (thread 0) returns the result. When it
!  finishes, it sets a global stop flag that makes the helpers unwind.
! =============================================================================
module chess_search_mod
  use, intrinsic :: iso_fortran_env, only: int32, int64
  use, intrinsic :: iso_c_binding
  use omp_lib
  use chess_engine
  implicit none
  private

  public :: search_info_t
  public :: init_search, clear_tt, evaluate, search_position, set_num_threads
  public :: chess_search_c, chess_eval_c, chess_clear_tt_c, chess_set_threads_c
  public :: MATE_SCORE, MATE_THRESHOLD, INF_SCORE

  integer, parameter :: INF_SCORE = 30000
  integer, parameter :: MATE_SCORE = 29000
  integer, parameter :: MATE_THRESHOLD = 28000
  integer, parameter :: MAX_PLY = 128

  ! Transposition table: 2^24 entries (2 x 128 MB)
  integer, parameter :: TT_BITS = 24
  integer, parameter :: TT_SIZE = ishft(1, TT_BITS)
  integer(int64), parameter :: TT_MASK = int(TT_SIZE - 1, int64)
  integer(int64), parameter :: MOVE_MASK = 262143_int64   ! 18 bits

  integer, parameter :: TT_NONE = 0, TT_EXACT = 1, TT_ALPHA = 2, TT_BETA = 3

  ! Search tuning
  integer, parameter :: RAZOR_DEPTH = 3, RAZOR_BASE = 300, RAZOR_STEP = 150
  integer, parameter :: NULL_MIN_DEPTH = 3, NULL_R_BASE = 3
  integer, parameter :: FUT_DEPTH = 4, FUT_STEP = 150
  integer, parameter :: DELTA_MARGIN = 200
  integer, parameter :: LMR_MIN_DEPTH = 3, LMR_MIN_MOVE = 4
  integer, parameter :: ASP_MIN_DEPTH = 5, ASP_WINDOW = 40
  integer, parameter :: HIST_MAX = 16384

  integer(int64), allocatable, save :: tt_key(:), tt_dat(:)
  integer, save :: LMR_TAB(0:MAX_PLY, 0:MAX_MOVES - 1)
  logical, save :: search_initialised = .false.
  integer, save :: g_num_threads = 0       ! 0 = OpenMP default
  integer, save :: g_stop = 0              ! shared stop flag for Lazy SMP

  type :: search_info_t
    integer(int32) :: best_move = 0_int32
    integer(int32) :: score = 0_int32
    integer(int64) :: nodes = 0_int64
    integer :: depth = 0
  end type search_info_t

  type :: thread_ctx_t
    integer :: tid = 0
    integer(int64) :: nodes = 0_int64
    logical :: aborted = .false.
    integer(int32), allocatable :: mv(:, :)        ! move list per ply
    integer(int32), allocatable :: sc(:, :)        ! move scores per ply
    integer(int32) :: killers(2, 0:MAX_PLY) = 0_int32
    integer(int32), allocatable :: history(:, :, :)
  end type thread_ctx_t

contains

  ! ---------------------------------------------------------------------------
  !  Shared stop flag
  ! ---------------------------------------------------------------------------
  logical function stop_requested()
    integer :: s
    !$omp atomic read
    s = g_stop
    stop_requested = (s /= 0)
  end function stop_requested

  subroutine set_stop(v)
    integer, intent(in) :: v
    !$omp atomic write
    g_stop = v
  end subroutine set_stop

  integer function effective_threads()
    if (g_num_threads > 0) then
      effective_threads = g_num_threads
    else
      effective_threads = omp_get_max_threads()
    end if
  end function effective_threads

  subroutine set_num_threads(n)
    integer, intent(in) :: n
    g_num_threads = n
  end subroutine set_num_threads

  ! ---------------------------------------------------------------------------
  !  Initialisation
  ! ---------------------------------------------------------------------------
  subroutine init_search()
    integer :: d, k

    if (search_initialised) return
    call init_engine()

    if (.not. allocated(tt_key)) then
      allocate (tt_key(0:TT_SIZE - 1), tt_dat(0:TT_SIZE - 1))
    end if
    tt_key = 0_int64
    tt_dat = 0_int64

    do d = 0, MAX_PLY
      do k = 0, MAX_MOVES - 1
        if (d == 0 .or. k == 0) then
          LMR_TAB(d, k) = 0
        else
          LMR_TAB(d, k) = int(0.75 + log(real(d)) * log(real(k)) / 2.25)
        end if
      end do
    end do

    search_initialised = .true.
  end subroutine init_search

  subroutine clear_tt()
    if (allocated(tt_key)) then
      tt_key = 0_int64
      tt_dat = 0_int64
    end if
  end subroutine clear_tt

  pure integer function evaluate(p)
    type(position_t), intent(in) :: p
    evaluate = static_eval(p)
  end function evaluate

  ! ---------------------------------------------------------------------------
  !  Transposition table (lock-free, XOR-verified)
  !  data layout: bits 0..17 move, 18..33 score+32768, 34..41 depth, 42..43 flag
  ! ---------------------------------------------------------------------------
  subroutine tt_probe(hash, ply, found, t_depth, t_flag, t_score, t_move)
    integer(int64), intent(in) :: hash
    integer, intent(in) :: ply
    logical, intent(out) :: found
    integer, intent(out) :: t_depth, t_flag, t_score
    integer(int32), intent(out) :: t_move
    integer(int64) :: idx, k, d
    integer :: s

    found = .false.
    t_depth = -1
    t_flag = TT_NONE
    t_score = 0
    t_move = 0_int32

    idx = iand(hash, TT_MASK)
    k = tt_key(idx)
    d = tt_dat(idx)
    if (ieor(k, d) /= hash) return

    t_flag = int(iand(ishft(d, -42), 3_int64))
    if (t_flag == TT_NONE) return

    found = .true.
    t_move = int(iand(d, MOVE_MASK), int32)
    t_depth = int(iand(ishft(d, -34), 255_int64))
    s = int(iand(ishft(d, -18), 65535_int64)) - 32768
    if (s > MATE_THRESHOLD) then
      s = s - ply
    else if (s < -MATE_THRESHOLD) then
      s = s + ply
    end if
    t_score = s
  end subroutine tt_probe

  subroutine tt_store(hash, depth, ply, score, flag, move)
    integer(int64), intent(in) :: hash
    integer, intent(in) :: depth, ply, score, flag
    integer(int32), intent(in) :: move
    integer(int64) :: idx, d_old, d_new
    integer :: s, dep, old_depth
    integer(int32) :: mv
    logical :: same

    idx = iand(hash, TT_MASK)
    d_old = tt_dat(idx)
    same = (ieor(tt_key(idx), d_old) == hash)
    dep = min(max(depth, 0), 255)
    mv = move

    if (same) then
      old_depth = int(iand(ishft(d_old, -34), 255_int64))
      if (dep < old_depth) return                  ! keep the deeper entry
      if (mv == 0_int32) mv = int(iand(d_old, MOVE_MASK), int32)
    end if

    s = max(-32000, min(32000, score))
    if (s > MATE_THRESHOLD) s = s + ply
    if (s < -MATE_THRESHOLD) s = s - ply

    d_new = iand(int(mv, int64), MOVE_MASK)
    d_new = ior(d_new, ishft(int(s + 32768, int64), 18))
    d_new = ior(d_new, ishft(int(dep, int64), 34))
    d_new = ior(d_new, ishft(int(flag, int64), 42))

    tt_dat(idx) = d_new
    tt_key(idx) = ieor(hash, d_new)
  end subroutine tt_store

  ! ---------------------------------------------------------------------------
  !  Move ordering
  ! ---------------------------------------------------------------------------
  pure subroutine hist_update(h, us, from, to, bonus)
    integer(int32), intent(inout) :: h(0:1, 0:63, 0:63)
    integer, intent(in) :: us, from, to, bonus
    integer(int32) :: cur
    cur = h(us, from, to)
    h(us, from, to) = cur + bonus - (cur * abs(bonus)) / HIST_MAX
  end subroutine hist_update

  subroutine score_moves(p, mv, sc, n, tt_move, k1, k2, hist, us)
    type(position_t), intent(in) :: p
    integer(int32), intent(in) :: mv(:)
    integer(int32), intent(inout) :: sc(:)
    integer, intent(in) :: n, us
    integer(int32), intent(in) :: tt_move, k1, k2
    integer(int32), intent(in) :: hist(0:1, 0:63, 0:63)
    integer :: i, from, to, promo, att, vic
    integer(int32) :: m

    do i = 1, n
      m = mv(i)
      from = iand(m, 63)
      to = iand(ishft(m, -6), 63)
      promo = iand(ishft(m, -12), 7)

      if (tt_move /= 0_int32 .and. m == tt_move) then
        sc(i) = 3000000
      else if (iand(m, MF_CAPTURE) /= 0) then
        att = piece_type_of(int(p%piece_on(from)))
        if (iand(m, MF_EP) /= 0) then
          vic = PAWN
        else
          vic = piece_type_of(int(p%piece_on(to)))
        end if
        sc(i) = 1000000 + 10*PIECE_VAL(vic) - PIECE_VAL(att)
        if (promo /= 0) sc(i) = sc(i) + PIECE_VAL(promo)
      else if (promo /= 0) then
        sc(i) = 1500000 + PIECE_VAL(promo)
      else if (m == k1) then
        sc(i) = 900000
      else if (m == k2) then
        sc(i) = 890000
      else
        sc(i) = hist(us, from, to)
      end if
    end do
  end subroutine score_moves

  ! Root move noise for Lazy SMP helper threads
  subroutine diversify_root(mv, sc, n, tid)
    integer(int32), intent(in) :: mv(:)
    integer(int32), intent(inout) :: sc(:)
    integer, intent(in) :: n, tid
    integer :: i
    integer(int64) :: noise
    do i = 1, n
      noise = mod(int(mv(i), int64) * 2654435761_int64 + int(tid, int64) * 40503_int64, 997_int64)
      sc(i) = sc(i) + int(noise, int32)
    end do
  end subroutine diversify_root

  ! Selection step: bring the best remaining move to position idx
  subroutine pick_move(mv, sc, n, idx)
    integer(int32), intent(inout) :: mv(:)
    integer(int32), intent(inout) :: sc(:)
    integer, intent(in) :: n, idx
    integer :: j, best_idx
    integer(int32) :: best_s, tmp_s, tmp_m

    best_idx = idx
    best_s = sc(idx)
    do j = idx + 1, n
      if (sc(j) > best_s) then
        best_s = sc(j)
        best_idx = j
      end if
    end do

    if (best_idx /= idx) then
      tmp_m = mv(idx)
      mv(idx) = mv(best_idx)
      mv(best_idx) = tmp_m
      tmp_s = sc(idx)
      sc(idx) = sc(best_idx)
      sc(best_idx) = tmp_s
    end if
  end subroutine pick_move

  ! ---------------------------------------------------------------------------
  !  Quiescence search (captures and promotions; all evasions when in check)
  ! ---------------------------------------------------------------------------
  recursive integer function qsearch(p, alpha_in, beta, ply, ctx) result(best)
    type(position_t), intent(inout) :: p
    integer, intent(in) :: alpha_in, beta, ply
    type(thread_ctx_t), intent(inout) :: ctx
    integer :: alpha, stand, score, i, n, legal, us, victim, to
    integer(int32) :: m
    logical :: in_check, is_cap, is_promo
    type(undo_t) :: u

    ctx%nodes = ctx%nodes + 1_int64
    if (iand(ctx%nodes, 2047_int64) == 0_int64) then
      if (stop_requested()) ctx%aborted = .true.
    end if
    if (ctx%aborted) then
      best = 0
      return
    end if
    if (ply >= MAX_PLY) then
      best = static_eval(p)
      return
    end if

    us = p%side
    alpha = alpha_in
    in_check = is_attacked(p, king_square(p, us), 1 - us)
    stand = -INF_SCORE

    if (in_check) then
      best = -INF_SCORE
    else
      stand = static_eval(p)
      if (stand >= beta) then
        best = stand
        return
      end if
      if (stand > alpha) alpha = stand
      best = stand
    end if

    n = 0
    call gen_pseudo_arr(p, ctx%mv(:, ply), n)
    call score_moves(p, ctx%mv(:, ply), ctx%sc(:, ply), n, 0_int32, 0_int32, 0_int32, &
                     ctx%history, us)

    legal = 0
    do i = 1, n
      call pick_move(ctx%mv(:, ply), ctx%sc(:, ply), n, i)
      m = ctx%mv(i, ply)
      is_cap = iand(m, MF_CAPTURE) /= 0
      is_promo = iand(m, MF_PROMO_MASK) /= 0

      ! Captures and promotions are sorted first; quiet moves end the list
      ! when we are not in check.
      if (.not. in_check .and. .not. is_cap .and. .not. is_promo) exit

      to = iand(ishft(m, -6), 63)
      if (.not. in_check .and. .not. is_promo) then
        if (iand(m, MF_EP) /= 0) then
          victim = PIECE_VAL(PAWN)
        else
          victim = PIECE_VAL(piece_type_of(int(p%piece_on(to))))
        end if
        if (stand + victim + DELTA_MARGIN <= alpha) cycle
      end if

      call do_move(p, m, u)
      if (is_attacked(p, king_square(p, us), 1 - us)) then
        call undo_move(p, m, u)
        cycle
      end if
      legal = legal + 1

      score = -qsearch(p, -beta, -alpha, ply + 1, ctx)
      call undo_move(p, m, u)

      if (ctx%aborted) then
        best = 0
        return
      end if

      if (score > best) then
        best = score
        if (score > alpha) then
          alpha = score
          if (alpha >= beta) return
        end if
      end if
    end do

    if (in_check .and. legal == 0) best = -MATE_SCORE + ply
  end function qsearch

  ! ---------------------------------------------------------------------------
  !  Negamax PVS with pruning and reductions
  ! ---------------------------------------------------------------------------
  recursive integer function negamax(p, depth, alpha_in, beta_in, ply, ctx, allow_null, best_out) &
      result(best)
    type(position_t), intent(inout) :: p
    integer, intent(in) :: depth, alpha_in, beta_in, ply
    type(thread_ctx_t), intent(inout) :: ctx
    logical, intent(in) :: allow_null
    integer(int32), intent(out) :: best_out

    integer :: alpha, bta, orig_alpha, sev, fut_margin, n, legal, us, them
    integer :: i, j, r, newd, ext, score, bonus, from, to, flag
    integer :: t_depth, t_flag, t_score
    integer(int32) :: tt_move, m, best_m, dummy_m, mj
    logical :: found, in_check, pv, fut, is_cap, is_promo, quiet, gives
    type(undo_t) :: u

    best = -INF_SCORE
    best_out = 0_int32
    dummy_m = 0_int32

    ctx%nodes = ctx%nodes + 1_int64
    if (iand(ctx%nodes, 2047_int64) == 0_int64) then
      if (stop_requested()) ctx%aborted = .true.
    end if
    if (ctx%aborted) then
      best = 0
      return
    end if

    if (ply >= MAX_PLY) then
      best = static_eval(p)
      return
    end if

    us = p%side
    them = 1 - us
    alpha = alpha_in
    bta = beta_in

    if (ply > 0) then
      if (p%halfmove >= 100) then
        best = 0
        return
      end if
      ! Mate-distance pruning
      alpha = max(alpha, -MATE_SCORE + ply)
      bta = min(bta, MATE_SCORE - ply - 1)
      if (alpha >= bta) then
        best = alpha
        return
      end if
    end if

    orig_alpha = alpha
    pv = (bta - alpha) > 1
    in_check = is_attacked(p, king_square(p, us), them)

    ! Transposition table
    call tt_probe(p%hash, ply, found, t_depth, t_flag, t_score, tt_move)
    if (found .and. ply > 0 .and. .not. pv .and. t_depth >= depth) then
      if (t_flag == TT_EXACT .or. (t_flag == TT_ALPHA .and. t_score <= alpha) &
          .or. (t_flag == TT_BETA .and. t_score >= bta)) then
        best = t_score
        return
      end if
    end if

    if (depth <= 0) then
      best = qsearch(p, alpha, bta, ply, ctx)
      return
    end if

    sev = 0
    if (.not. in_check) sev = static_eval(p)

    if (ply > 0 .and. .not. pv .and. .not. in_check) then
      ! Razoring
      if (depth <= RAZOR_DEPTH .and. sev + RAZOR_BASE + RAZOR_STEP*depth <= alpha) then
        score = qsearch(p, alpha, alpha + 1, ply, ctx)
        if (ctx%aborted) then
          best = 0
          return
        end if
        if (score <= alpha) then
          best = score
          return
        end if
      end if

      ! Null-move pruning
      if (allow_null .and. depth >= NULL_MIN_DEPTH .and. sev >= bta .and. has_non_pawns(p, us)) then
        r = NULL_R_BASE + depth / 4
        call do_null(p, u)
        score = -negamax(p, depth - 1 - r, -bta, -bta + 1, ply + 1, ctx, .false., dummy_m)
        call undo_null(p, u)
        if (ctx%aborted) then
          best = 0
          return
        end if
        if (score >= bta) then
          if (score >= MATE_THRESHOLD) then
            best = bta
          else
            best = score
          end if
          return
        end if
      end if
    end if

    fut = (ply > 0 .and. .not. pv .and. .not. in_check .and. depth <= FUT_DEPTH .and. &
           abs(alpha) < MATE_THRESHOLD)
    fut_margin = FUT_STEP * depth

    ! Move generation and ordering
    n = 0
    call gen_pseudo_arr(p, ctx%mv(:, ply), n)
    call score_moves(p, ctx%mv(:, ply), ctx%sc(:, ply), n, tt_move, &
                     ctx%killers(1, ply), ctx%killers(2, ply), ctx%history, us)
    if (ply == 0 .and. ctx%tid > 0) then
      call diversify_root(ctx%mv(:, ply), ctx%sc(:, ply), n, ctx%tid)
    end if

    legal = 0
    best_m = 0_int32

    do i = 1, n
      call pick_move(ctx%mv(:, ply), ctx%sc(:, ply), n, i)
      m = ctx%mv(i, ply)
      is_cap = iand(m, MF_CAPTURE) /= 0
      is_promo = iand(m, MF_PROMO_MASK) /= 0
      quiet = .not. (is_cap .or. is_promo)
      from = iand(m, 63)
      to = iand(ishft(m, -6), 63)

      call do_move(p, m, u)
      if (is_attacked(p, king_square(p, us), them)) then
        call undo_move(p, m, u)
        cycle
      end if
      legal = legal + 1
      gives = is_attacked(p, king_square(p, them), us)

      ! Futility pruning for quiet moves
      if (fut .and. quiet .and. .not. gives .and. legal > 1 .and. sev + fut_margin <= alpha) then
        call undo_move(p, m, u)
        cycle
      end if

      ext = 0
      if (gives) ext = 1
      newd = depth - 1 + ext

      if (legal == 1) then
        score = -negamax(p, newd, -bta, -alpha, ply + 1, ctx, .true., dummy_m)
      else
        ! Late move reductions
        r = 0
        if (depth >= LMR_MIN_DEPTH .and. legal >= LMR_MIN_MOVE .and. quiet .and. &
            .not. in_check .and. .not. gives) then
          r = LMR_TAB(min(depth, MAX_PLY), min(legal, MAX_MOVES - 1))
          if (m == ctx%killers(1, ply) .or. m == ctx%killers(2, ply)) r = r - 1
          r = max(0, min(r, newd - 1))
        end if

        score = -negamax(p, newd - r, -alpha - 1, -alpha, ply + 1, ctx, .true., dummy_m)
        if (score > alpha .and. r > 0) then
          score = -negamax(p, newd, -alpha - 1, -alpha, ply + 1, ctx, .true., dummy_m)
        end if
        if (score > alpha .and. score < bta) then
          score = -negamax(p, newd, -bta, -alpha, ply + 1, ctx, .true., dummy_m)
        end if
      end if

      call undo_move(p, m, u)
      if (ctx%aborted) then
        best = 0
        return
      end if

      if (score > best) then
        best = score
        best_m = m
        if (score > alpha) then
          alpha = score
          if (alpha >= bta) then
            if (quiet) then
              bonus = min(depth*depth, 400)
              if (m /= ctx%killers(1, ply)) then
                ctx%killers(2, ply) = ctx%killers(1, ply)
                ctx%killers(1, ply) = m
              end if
              call hist_update(ctx%history, us, from, to, bonus)
              ! Penalise quiet moves that were searched before the cutoff move
              do j = 1, i - 1
                mj = ctx%mv(j, ply)
                if (iand(mj, MF_CAPTURE) == 0 .and. iand(mj, MF_PROMO_MASK) == 0 .and. mj /= m) then
                  call hist_update(ctx%history, us, iand(mj, 63), iand(ishft(mj, -6), 63), -bonus)
                end if
              end do
            end if
            exit
          end if
        end if
      end if
    end do

    if (legal == 0) then
      if (in_check) then
        best = -MATE_SCORE + ply
      else
        best = 0
      end if
      best_out = 0_int32
      return
    end if

    best_out = best_m

    if (best <= orig_alpha) then
      flag = TT_ALPHA
    else if (best >= bta) then
      flag = TT_BETA
    else
      flag = TT_EXACT
    end if
    call tt_store(p%hash, depth, ply, best, flag, best_m)
  end function negamax

  ! ---------------------------------------------------------------------------
  !  Per-thread search state
  ! ---------------------------------------------------------------------------
  subroutine init_ctx(ctx, tid)
    type(thread_ctx_t), intent(out) :: ctx
    integer, intent(in) :: tid

    allocate (ctx%mv(MAX_MOVES, 0:MAX_PLY))
    allocate (ctx%sc(MAX_MOVES, 0:MAX_PLY))
    allocate (ctx%history(0:1, 0:63, 0:63))
    ctx%mv = 0_int32
    ctx%sc = 0_int32
    ctx%history = 0_int32
    ctx%killers = 0_int32
    ctx%tid = tid
    ctx%nodes = 0_int64
    ctx%aborted = .false.
  end subroutine init_ctx

  ! Iterative deepening for one thread
  subroutine thread_iterate(p0, max_depth, tid, out)
    type(position_t), intent(in) :: p0
    integer, intent(in) :: max_depth, tid
    type(search_info_t), intent(inout) :: out
    type(position_t) :: root
    type(thread_ctx_t) :: ctx
    integer :: d, alpha, beta, score, prev
    integer(int32) :: bm

    root = p0
    call init_ctx(ctx, tid)

    out%best_move = 0_int32
    out%score = 0_int32
    out%depth = 0
    prev = 0
    bm = 0_int32
    score = 0

    do d = 1, max_depth
      ! Helpers skip some depths so their TT contents differ from the main thread
      if (tid > 0 .and. d < max_depth .and. mod(d + tid, 3) == 0) cycle

      if (d >= ASP_MIN_DEPTH .and. out%depth > 0) then
        alpha = prev - ASP_WINDOW
        beta = prev + ASP_WINDOW
      else
        alpha = -INF_SCORE
        beta = INF_SCORE
      end if

      do
        score = negamax(root, d, alpha, beta, 0, ctx, .true., bm)
        if (ctx%aborted) exit
        if (score <= alpha .and. alpha > -INF_SCORE) then
          alpha = -INF_SCORE
        else if (score >= beta .and. beta < INF_SCORE) then
          beta = INF_SCORE
        else
          exit
        end if
      end do

      if (ctx%aborted) exit
      if (bm /= 0_int32) out%best_move = bm
      out%score = int(score, int32)
      out%depth = d
      prev = score
    end do

    out%nodes = ctx%nodes
    if (tid == 0) call set_stop(1)
  end subroutine thread_iterate

  ! ---------------------------------------------------------------------------
  !  Public driver: Lazy SMP over OpenMP threads
  ! ---------------------------------------------------------------------------
  subroutine search_position(p, max_depth, info)
    type(position_t), intent(in) :: p
    integer, intent(in) :: max_depth
    type(search_info_t), intent(out) :: info
    type(search_info_t), allocatable :: res(:)
    integer :: nt, t, md
    integer(int64) :: total

    call init_search()
    md = max(1, max_depth)
    nt = effective_threads()
    allocate (res(0:nt - 1))
    call set_stop(0)

!$omp parallel num_threads(nt) default(shared) private(t)
    t = omp_get_thread_num()
    call thread_iterate(p, md, t, res(t))
!$omp end parallel

    total = 0_int64
    do t = 0, nt - 1
      total = total + res(t)%nodes
    end do

    info = res(0)
    info%nodes = total
  end subroutine search_position

  ! ---------------------------------------------------------------------------
  !  C API
  ! ---------------------------------------------------------------------------
  subroutine chess_clear_tt_c() bind(C, name="chess_clear_tt")
    call init_search()
    call clear_tt()
  end subroutine chess_clear_tt_c

  subroutine chess_set_threads_c(n) bind(C, name="chess_set_threads")
    integer(c_int), value, intent(in) :: n
    call set_num_threads(int(n))
  end subroutine chess_set_threads_c

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
    score = int(static_eval(p), c_int32_t)
    chess_eval_c = 0_c_int
  end function chess_eval_c

  ! Search a FEN position to a fixed depth.
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
    score = 0_c_int32_t
    nodes = 0_c_int64_t

    call init_search()
    call parse_fen(c_to_f(fen), p, ok)
    if (.not. ok) then
      chess_search_c = -1_c_int
      return
    end if

    call search_position(p, int(depth), info)
    best_move = info%best_move
    score = info%score
    nodes = info%nodes
    chess_search_c = 0_c_int
  end function chess_search_c

end module chess_search_mod