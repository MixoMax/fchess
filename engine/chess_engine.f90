! =============================================================================
!  chess_engine.f90
!  Bitboard-based chess move generation (Fortran 2008)
!
!  Conventions
!    * Square index: sq = 8*rank + file, a1 = 0, h1 = 7, a8 = 56, h8 = 63.
!    * Each piece type of each colour is one 64-bit integer (bitboard).
!    * Colours: WHITE = 0, BLACK = 1.  Piece types: PAWN..KING = 1..6.
!      Bitboard index for a piece = 6*colour + type  (1..12).
!    * Castling rights bitmask: WK=1, WQ=2, BK=4, BQ=8.
!    * Move encoding (int32):
!        bits  0..5   from square
!        bits  6..11  to square
!        bits 12..14  promotion piece (2=N, 3=B, 4=R, 5=Q), 0 if none
!        bit  15      capture (includes en passant)
!        bit  16      en passant capture
!        bit  17      castling
!
!  Move generation: pseudo-legal moves are generated with bitboard attacks,
!  then each one is played on a copy of the position and discarded if it
!  leaves the mover's king attacked. This is simple and provably correct.
! =============================================================================
module chess_engine
  use, intrinsic :: iso_fortran_env, only: int32, int64
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public :: position_t, move_list_t, stats_t
  public :: WHITE, BLACK, PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
  public :: CR_WK, CR_WQ, CR_BK, CR_BQ
  public :: MF_CAPTURE, MF_EP, MF_CASTLE, MF_PROMO_MASK
  public :: init_engine, parse_fen, gen_legal, gen_pseudo, make_move, run_perft
  public :: move_to_uci, is_attacked, king_square, sqbit, compute_hash, c_to_f
  public :: chess_init, chess_perft, chess_legal_moves, chess_move_uci
  public :: chess_make_move, chess_in_check

  integer, parameter :: WHITE = 0, BLACK = 1
  integer, parameter :: PAWN = 1, KNIGHT = 2, BISHOP = 3, ROOK = 4, QUEEN = 5, KING = 6
  integer, parameter :: CR_WK = 1, CR_WQ = 2, CR_BK = 4, CR_BQ = 8

  integer, parameter :: MF_CAPTURE = 32768     ! bit 15       (binary: 001000000000000000)
  integer, parameter :: MF_EP = 65536          ! bit 16       (binary: 010000000000000000)
  integer, parameter :: MF_CASTLE = 131072     ! bit 17       (binary: 100000000000000000)
  integer, parameter :: MF_PROMO_MASK = 28672  ! bits 12..14  (binary: 000111000000000000)

  type :: position_t
    integer(int64) :: bb(12)       ! piece bitboards, 1..6 white, 7..12 black
    integer(int64) :: occ(0:1)     ! occupancy per colour
    integer(int64) :: occ_all      ! total occupancy
    integer(int64) :: hash = 0_int64 ! Zobrist hash key
    integer :: side = WHITE        ! side to move
    integer :: castling = 0        ! castling rights bitmask
    integer :: ep_sq = -1          ! en passant target square, -1 if none
    integer :: halfmove = 0
    integer :: fullmove = 1
  end type position_t

  type :: move_list_t
    integer :: n = 0
    integer(int32) :: m(512)
  end type move_list_t

  type :: stats_t
    integer(int64) :: nodes = 0_int64
    integer(int64) :: captures = 0_int64
    integer(int64) :: ep = 0_int64
    integer(int64) :: castles = 0_int64
    integer(int64) :: promotions = 0_int64
    integer(int64) :: checks = 0_int64
  end type stats_t

  ! Lookup tables, filled once by init_engine
  logical, save :: initialised = .false.
  integer(int64), save :: RAY(8, 0:63)          ! full ray from sq in direction d
  integer(int64), save :: KNIGHT_ATT(0:63)
  integer(int64), save :: KING_ATT(0:63)
  integer(int64), save :: PAWN_ATT(0:1, 0:63)   ! PAWN_ATT(colour, sq)
  integer, save :: CASTLE_MASK(0:63)

  ! Zobrist random keys, filled once by init_engine
  integer(int64), save :: ZOBRIST_PC(12, 0:63)
  integer(int64), save :: ZOBRIST_CASTLE(0:15)
  integer(int64), save :: ZOBRIST_EP(0:63)
  integer(int64), save :: ZOBRIST_SIDE

contains
  pure integer(int64) function sqbit(sq)
    integer, intent(in) :: sq
    sqbit = ishft(1_int64, sq)
  end function sqbit

  ! check if f and r are within the board (0..7)
  pure logical function inboard(f, r)
    integer, intent(in) :: f, r
    inboard = iand(ior(f, r), not(7)) == 0
  end function inboard

  ! Deterministic 64-bit xorshift PRNG for Zobrist table initialisation
  integer(int64) function next_rand(state)
    integer(int64), intent(inout) :: state
    state = ieor(state, ishft(state, 13))
    state = ieor(state, ishft(state, -7))
    state = ieor(state, ishft(state, 17))
    next_rand = state
  end function next_rand

  ! init tables
  subroutine init_engine()
    integer, parameter :: DF(8) = [0, 1, 1, -1, 0, -1, 1, -1]
    integer, parameter :: DR(8) = [1, 0, 1, 1, -1, 0, -1, -1]
    integer, parameter :: KDF(8) = [1, 2, 2, 1, -1, -2, -2, -1]
    integer, parameter :: KDR(8) = [2, 1, -1, -2, -2, -1, 1, 2]
    integer :: sq, f, r, d, k, nf, nr, pc
    integer(int64) :: seed

    if (initialised) return

    RAY = 0_int64
    KNIGHT_ATT = 0_int64
    KING_ATT = 0_int64
    PAWN_ATT = 0_int64

    CASTLE_MASK = 15
    CASTLE_MASK(0) = 13     ! a1 moved/captured: loses WQ
    CASTLE_MASK(7) = 14     ! h1: loses WK
    CASTLE_MASK(4) = 12     ! e1: loses WK and WQ
    CASTLE_MASK(56) = 7     ! a8: loses BQ
    CASTLE_MASK(60) = 3     ! e8: loses BK and BQ
    CASTLE_MASK(63) = 11    ! h8: loses BK

    do sq = 0, 63
      f = modulo(sq, 8)
      r = sq / 8

      ! Rays: directions 1..4 increase the square index, 5..8 decrease it
      do d = 1, 8
        nf = f
        nr = r
        do
          nf = nf + DF(d)
          nr = nr + DR(d)
          if (.not. inboard(nf, nr)) exit
          RAY(d, sq) = ior(RAY(d, sq), sqbit(nr*8 + nf))
        end do
      end do

      do k = 1, 8
        nf = f + KDF(k)
        nr = r + KDR(k)
        if (inboard(nf, nr)) KNIGHT_ATT(sq) = ior(KNIGHT_ATT(sq), sqbit(nr*8 + nf))

        nf = f + DF(k)
        nr = r + DR(k)
        if (inboard(nf, nr)) KING_ATT(sq) = ior(KING_ATT(sq), sqbit(nr*8 + nf))
      end do

      do k = -1, 1, 2
        nf = f + k
        nr = r + 1
        if (inboard(nf, nr)) PAWN_ATT(WHITE, sq) = ior(PAWN_ATT(WHITE, sq), sqbit(nr*8 + nf))
        nr = r - 1
        if (inboard(nf, nr)) PAWN_ATT(BLACK, sq) = ior(PAWN_ATT(BLACK, sq), sqbit(nr*8 + nf))
      end do
    end do

    ! Zobrist random keys (fixed seed for reproducible hashes)
    seed = 1070372_int64
    do sq = 0, 63
      do pc = 1, 12
        ZOBRIST_PC(pc, sq) = next_rand(seed)
      end do
      ZOBRIST_EP(sq) = next_rand(seed)
    end do
    do k = 0, 15
      ZOBRIST_CASTLE(k) = next_rand(seed)
    end do
    ZOBRIST_SIDE = next_rand(seed)

    initialised = .true.
  end subroutine init_engine

  ! ---------------------------------------------------------------------------
  !  Zobrist hash computation from scratch
  ! ---------------------------------------------------------------------------
  pure integer(int64) function compute_hash(p) result(h)
    type(position_t), intent(in) :: p
    integer :: pc, sq
    integer(int64) :: b

    h = 0_int64
    do pc = 1, 12
      b = p%bb(pc)
      do while (b /= 0_int64)
        sq = trailz(b)
        b = iand(b, b - 1_int64)
        h = ieor(h, ZOBRIST_PC(pc, sq))
      end do
    end do
    h = ieor(h, ZOBRIST_CASTLE(iand(p%castling, 15)))
    if (p%ep_sq >= 0) h = ieor(h, ZOBRIST_EP(p%ep_sq))
    if (p%side == BLACK) h = ieor(h, ZOBRIST_SIDE)
  end function compute_hash

  ! ---------------------------------------------------------------------------
  !  Sliding attacks using ray tables and the nearest blocker
  ! ---------------------------------------------------------------------------
  pure function ray_attack(d, sq, occ) result(a)
    integer, intent(in) :: d, sq
    integer(int64), intent(in) :: occ
    integer(int64) :: a, blk
    integer :: s

    a = RAY(d, sq)
    blk = iand(a, occ)
    if (blk == 0_int64) return
    if (d <= 4) then
      s = trailz(blk)          ! nearest blocker has the lowest index
    else
      s = 63 - leadz(blk)      ! nearest blocker has the highest index
    end if
    ! Ray beyond the blocker is removed; the blocker itself stays attacked.
    a = ieor(a, RAY(d, s))
  end function ray_attack

  pure function bishop_attacks(sq, occ) result(a)
    integer, intent(in) :: sq
    integer(int64), intent(in) :: occ
    integer(int64) :: a
    a = ior(ior(ray_attack(3, sq, occ), ray_attack(4, sq, occ)), &
            ior(ray_attack(7, sq, occ), ray_attack(8, sq, occ)))
  end function bishop_attacks

  pure function rook_attacks(sq, occ) result(a)
    integer, intent(in) :: sq
    integer(int64), intent(in) :: occ
    integer(int64) :: a
    a = ior(ior(ray_attack(1, sq, occ), ray_attack(2, sq, occ)), &
            ior(ray_attack(5, sq, occ), ray_attack(6, sq, occ)))
  end function rook_attacks

  ! ---------------------------------------------------------------------------
  !  Position utilities
  ! ---------------------------------------------------------------------------
  subroutine update_occ(p)
    type(position_t), intent(inout) :: p
    integer :: c, k
    p%occ = 0_int64
    do c = 0, 1
      do k = 1, 6
        p%occ(c) = ior(p%occ(c), p%bb(6*c + k))
      end do
    end do
    p%occ_all = ior(p%occ(0), p%occ(1))
  end subroutine update_occ

  pure integer function king_square(p, side)
    type(position_t), intent(in) :: p
    integer, intent(in) :: side
    king_square = trailz(p%bb(6*side + KING))
  end function king_square

  ! True if square sq is attacked by any piece of colour 'by'
  pure logical function is_attacked(p, sq, by)
    type(position_t), intent(in) :: p
    integer, intent(in) :: sq, by
    integer :: o
    integer(int64) :: occ
    o = 6*by
    occ = p%occ_all
    is_attacked = (iand(p%bb(o + PAWN), PAWN_ATT(1 - by, sq)) /= 0_int64) .or. &
                  (iand(p%bb(o + KNIGHT), KNIGHT_ATT(sq)) /= 0_int64) .or. &
                  (iand(p%bb(o + KING), KING_ATT(sq)) /= 0_int64) .or. &
                  (iand(ior(p%bb(o + BISHOP), p%bb(o + QUEEN)), bishop_attacks(sq, occ)) /= 0_int64) .or. &
                  (iand(ior(p%bb(o + ROOK), p%bb(o + QUEEN)), rook_attacks(sq, occ)) /= 0_int64)
  end function is_attacked

  pure integer function piece_index(ch)
    character(len=1), intent(in) :: ch
    select case (ch)
    case ('P'); piece_index = PAWN
    case ('N'); piece_index = KNIGHT
    case ('B'); piece_index = BISHOP
    case ('R'); piece_index = ROOK
    case ('Q'); piece_index = QUEEN
    case ('K'); piece_index = KING
    case ('p'); piece_index = PAWN + 6
    case ('n'); piece_index = KNIGHT + 6
    case ('b'); piece_index = BISHOP + 6
    case ('r'); piece_index = ROOK + 6
    case ('q'); piece_index = QUEEN + 6
    case ('k'); piece_index = KING + 6
    case default; piece_index = 0
    end select
  end function piece_index

  ! Split a string into whitespace-separated tokens
  subroutine split_tokens(s, toks, n)
    character(len=*), intent(in) :: s
    character(len=*), intent(out) :: toks(:)
    integer, intent(out) :: n
    integer :: i, j
    n = 0
    i = 1
    do while (i <= len(s))
      if (s(i:i) == ' ' .or. s(i:i) == achar(9)) then
        i = i + 1
        cycle
      end if
      j = i
      do while (j <= len(s))
        if (s(j:j) == ' ' .or. s(j:j) == achar(9)) exit
        j = j + 1
      end do
      n = n + 1
      if (n <= size(toks)) toks(n) = s(i:j - 1)
      i = j
    end do
  end subroutine split_tokens


  subroutine parse_fen(fen, p, ok)
    character(len=*), intent(in) :: fen
    type(position_t), intent(out) :: p
    logical, intent(out) :: ok
    character(len=128) :: t(6)
    integer :: n, i, rank, file, sq, idx, ios
    character(len=1) :: ch

    call init_engine()

    ok = .false.
    p%bb = 0_int64
    p%hash = 0_int64
    p%side = WHITE
    p%castling = 0
    p%ep_sq = -1
    p%halfmove = 0
    p%fullmove = 1

    call split_tokens(fen, t, n)
    if (n < 4) return

    ! Piece placement
    rank = 7
    file = 0
    do i = 1, len_trim(t(1))
      ch = t(1)(i:i)
      if (ch == '/') then
        rank = rank - 1
        file = 0
      else if (ch >= '1' .and. ch <= '8') then
        file = file + ichar(ch) - ichar('0')
      else
        idx = piece_index(ch)
        if (idx == 0 .or. rank < 0 .or. rank > 7 .or. file > 7) return
        sq = rank*8 + file
        p%bb(idx) = ior(p%bb(idx), sqbit(sq))
        file = file + 1
      end if
    end do

    ! Side to move
    select case (trim(t(2)))
    case ('w'); p%side = WHITE
    case ('b'); p%side = BLACK
    case default; return
    end select

    ! Castling rights
    if (trim(t(3)) /= '-') then
      do i = 1, len_trim(t(3))
        select case (t(3)(i:i))
        case ('K'); p%castling = ior(p%castling, CR_WK)
        case ('Q'); p%castling = ior(p%castling, CR_WQ)
        case ('k'); p%castling = ior(p%castling, CR_BK)
        case ('q'); p%castling = ior(p%castling, CR_BQ)
        case default; return
        end select
      end do
    end if

    ! En passant target square
    if (trim(t(4)) /= '-') then
      if (len_trim(t(4)) < 2) return
      file = ichar(t(4)(1:1)) - ichar('a')
      rank = ichar(t(4)(2:2)) - ichar('1')
      if (file < 0 .or. file > 7 .or. rank < 0 .or. rank > 7) return
      p%ep_sq = rank*8 + file
    end if

    if (n >= 5) then
      read (t(5), *, iostat=ios) p%halfmove
      if (ios /= 0) p%halfmove = 0
    end if
    if (n >= 6) then
      read (t(6), *, iostat=ios) p%fullmove
      if (ios /= 0) p%fullmove = 1
    end if

    call update_occ(p)
    p%hash = compute_hash(p)
    ok = .true.
  end subroutine parse_fen

  subroutine add_move(ml, from, to, flags, promo)
    type(move_list_t), intent(inout) :: ml
    integer, intent(in) :: from, to, flags, promo
    ml%n = ml%n + 1
    ml%m(ml%n) = int(from + 64*to + 4096*promo + flags, int32)
  end subroutine add_move

  ! Push a pawn move; expands to four promotions when reaching the last rank
  subroutine push_pawn(ml, from, to, flags, promo_rank)
    type(move_list_t), intent(inout) :: ml
    integer, intent(in) :: from, to, flags, promo_rank
    integer :: pt
    if (to / 8 == promo_rank) then
      do pt = KNIGHT, QUEEN
        call add_move(ml, from, to, flags, pt)
      end do
    else
      call add_move(ml, from, to, flags, 0)
    end if
  end subroutine push_pawn

  ! Add moves from one square to every target bit in tb
  subroutine add_targets(ml, from, tb_in, enemy)
    type(move_list_t), intent(inout) :: ml
    integer, intent(in) :: from
    integer(int64), intent(in) :: tb_in, enemy
    integer(int64) :: tb
    integer :: to
    tb = tb_in
    do while (tb /= 0_int64)
      to = trailz(tb)
      tb = iand(tb, tb - 1_int64)
      if (iand(enemy, sqbit(to)) /= 0_int64) then
        call add_move(ml, from, to, MF_CAPTURE, 0)
      else
        call add_move(ml, from, to, 0, 0)
      end if
    end do
  end subroutine add_targets

  !  Castling (king and rook must be in place, path empty, and no attacks)
  subroutine gen_castles(p, ml)
    type(position_t), intent(in) :: p
    type(move_list_t), intent(inout) :: ml
    integer :: us, them, ksq, kside, qside
    integer(int64) :: occ

    us = p%side
    them = 1 - us
    if (us == WHITE) then
      ksq = 4
      kside = CR_WK
      qside = CR_WQ
    else
      ksq = 60
      kside = CR_BK
      qside = CR_BQ
    end if

    if (iand(p%bb(6*us + KING), sqbit(ksq)) == 0_int64) return
    if (is_attacked(p, ksq, them)) return
    occ = p%occ_all

    if (iand(p%castling, kside) /= 0) then
      if (iand(occ, ior(sqbit(ksq + 1), sqbit(ksq + 2))) == 0_int64 .and. &
          iand(p%bb(6*us + ROOK), sqbit(ksq + 3)) /= 0_int64) then
        if (.not. is_attacked(p, ksq + 1, them) .and. .not. is_attacked(p, ksq + 2, them)) then
          call add_move(ml, ksq, ksq + 2, MF_CASTLE, 0)
        end if
      end if
    end if

    if (iand(p%castling, qside) /= 0) then
      if (iand(occ, ior(sqbit(ksq - 1), ior(sqbit(ksq - 2), sqbit(ksq - 3)))) == 0_int64 .and. &
          iand(p%bb(6*us + ROOK), sqbit(ksq - 4)) /= 0_int64) then
        if (.not. is_attacked(p, ksq - 1, them) .and. .not. is_attacked(p, ksq - 2, them)) then
          call add_move(ml, ksq, ksq - 2, MF_CASTLE, 0)
        end if
      end if
    end if
  end subroutine gen_castles

  !  Pseudo-legal move generation
  recursive subroutine gen_pseudo(p, ml)
    type(position_t), intent(in) :: p
    type(move_list_t), intent(inout) :: ml
    integer :: us, them, ou, from, to, fwd, start_rank, promo_rank
    integer(int64) :: own, enemy, occ, pw, tb, b

    us = p%side
    them = 1 - us
    ou = 6*us
    own = p%occ(us)
    enemy = p%occ(them)
    occ = p%occ_all

    ! ---- pawns ----
    if (us == WHITE) then
      fwd = 8
      start_rank = 1
      promo_rank = 7
    else
      fwd = -8
      start_rank = 6
      promo_rank = 0
    end if

    pw = p%bb(ou + PAWN)
    do while (pw /= 0_int64)
      from = trailz(pw)
      pw = iand(pw, pw - 1_int64)

      to = from + fwd
      if (iand(occ, sqbit(to)) == 0_int64) then
        call push_pawn(ml, from, to, 0, promo_rank)
        if (from / 8 == start_rank) then
          if (iand(occ, sqbit(to + fwd)) == 0_int64) call add_move(ml, from, to + fwd, 0, 0)
        end if
      end if

      tb = iand(PAWN_ATT(us, from), enemy)
      do while (tb /= 0_int64)
        to = trailz(tb)
        tb = iand(tb, tb - 1_int64)
        call push_pawn(ml, from, to, MF_CAPTURE, promo_rank)
      end do

      if (p%ep_sq >= 0) then
        if (iand(PAWN_ATT(us, from), sqbit(p%ep_sq)) /= 0_int64) then
          call add_move(ml, from, p%ep_sq, MF_CAPTURE + MF_EP, 0)
        end if
      end if
    end do

    ! ---- knights ----
    b = p%bb(ou + KNIGHT)
    do while (b /= 0_int64)
      from = trailz(b)
      b = iand(b, b - 1_int64)
      call add_targets(ml, from, iand(KNIGHT_ATT(from), not(own)), enemy)
    end do

    ! ---- bishops ----
    b = p%bb(ou + BISHOP)
    do while (b /= 0_int64)
      from = trailz(b)
      b = iand(b, b - 1_int64)
      call add_targets(ml, from, iand(bishop_attacks(from, occ), not(own)), enemy)
    end do

    ! ---- rooks ----
    b = p%bb(ou + ROOK)
    do while (b /= 0_int64)
      from = trailz(b)
      b = iand(b, b - 1_int64)
      call add_targets(ml, from, iand(rook_attacks(from, occ), not(own)), enemy)
    end do

    ! ---- queens ----
    b = p%bb(ou + QUEEN)
    do while (b /= 0_int64)
      from = trailz(b)
      b = iand(b, b - 1_int64)
      call add_targets(ml, from, &
                       iand(ior(bishop_attacks(from, occ), rook_attacks(from, occ)), not(own)), enemy)
    end do

    ! ---- king ----
    b = p%bb(ou + KING)
    if (b /= 0_int64) then
      from = trailz(b)
      call add_targets(ml, from, iand(KING_ATT(from), not(own)), enemy)
    end if

    if (p%castling /= 0) call gen_castles(p, ml)
  end subroutine gen_pseudo


  !  Make a move (does not operate in place, returns a new position)
  function make_move(p, m) result(np)
    type(position_t), intent(in) :: p
    integer(int32), intent(in) :: m
    type(position_t) :: np
    integer :: from, to, us, them, ou, ot, pc, k, promo, cap_sq, rf, rt
    integer(int64) :: fb, tb, h
    logical :: is_cap, is_ep, is_castle

    np = p
    h = p%hash
    from = iand(m, 63)
    to = iand(ishft(m, -6), 63)
    promo = iand(ishft(m, -12), 7)
    us = p%side
    them = 1 - us
    ou = 6*us
    ot = 6*them
    fb = sqbit(from)
    tb = sqbit(to)
    is_cap = iand(m, MF_CAPTURE) /= 0
    is_ep = iand(m, MF_EP) /= 0
    is_castle = iand(m, MF_CASTLE) /= 0

    ! Remove previous en passant and castling keys from hash
    if (p%ep_sq >= 0) h = ieor(h, ZOBRIST_EP(p%ep_sq))
    h = ieor(h, ZOBRIST_CASTLE(iand(p%castling, 15)))

    ! Identify and remove the moving piece
    pc = 0
    do k = 1, 6
      if (iand(p%bb(ou + k), fb) /= 0_int64) then
        pc = k
        exit
      end if
    end do
    np%bb(ou + pc) = ieor(np%bb(ou + pc), fb)
    h = ieor(h, ZOBRIST_PC(ou + pc, from))

    ! Remove the captured piece
    if (is_ep) then
      cap_sq = to - 8 + 16*us
      np%bb(ot + PAWN) = ieor(np%bb(ot + PAWN), sqbit(cap_sq))
      h = ieor(h, ZOBRIST_PC(ot + PAWN, cap_sq))
    else if (is_cap) then
      do k = 1, 6
        if (iand(p%bb(ot + k), tb) /= 0_int64) then
          np%bb(ot + k) = ieor(np%bb(ot + k), tb)
          h = ieor(h, ZOBRIST_PC(ot + k, to))
          exit
        end if
      end do
    end if

    ! Place the moving (or promoted) piece
    if (promo /= 0) then
      np%bb(ou + promo) = ior(np%bb(ou + promo), tb)
      h = ieor(h, ZOBRIST_PC(ou + promo, to))
    else
      np%bb(ou + pc) = ior(np%bb(ou + pc), tb)
      h = ieor(h, ZOBRIST_PC(ou + pc, to))
    end if

    ! Move the rook for castling
    if (is_castle) then
      if (to > from) then
        rf = to + 1
        rt = to - 1
      else
        rf = to - 2
        rt = to + 1
      end if
      np%bb(ou + ROOK) = ieor(np%bb(ou + ROOK), ior(sqbit(rf), sqbit(rt)))
      h = ieor(h, ieor(ZOBRIST_PC(ou + ROOK, rf), ZOBRIST_PC(ou + ROOK, rt)))
    end if

    ! Update castling rights, en passant square, and clocks
    np%castling = iand(np%castling, iand(CASTLE_MASK(from), CASTLE_MASK(to)))
    h = ieor(h, ZOBRIST_CASTLE(iand(np%castling, 15)))

    if (pc == PAWN .and. abs(to - from) == 16) then
      np%ep_sq = (from + to) / 2
      h = ieor(h, ZOBRIST_EP(np%ep_sq))
    else
      np%ep_sq = -1
    end if

    if (pc == PAWN .or. is_cap) then
      np%halfmove = 0
    else
      np%halfmove = p%halfmove + 1
    end if
    if (us == BLACK) np%fullmove = p%fullmove + 1

    np%side = them
    h = ieor(h, ZOBRIST_SIDE)
    np%hash = h
    call update_occ(np)
  end function make_move

  !  Legal move generation: pseudo-legal moves filtered by king safety
  recursive subroutine gen_legal(p, ml)
    type(position_t), intent(in) :: p
    type(move_list_t), intent(out) :: ml
    type(move_list_t) :: pl
    type(position_t) :: np
    integer :: i

    pl%n = 0
    call gen_pseudo(p, pl)
    ml%n = 0
    do i = 1, pl%n
      np = make_move(p, pl%m(i))
      if (.not. is_attacked(np, king_square(np, p%side), 1 - p%side)) then
        ml%n = ml%n + 1
        ml%m(ml%n) = pl%m(i)
      end if
    end do
  end subroutine gen_legal

  !  Perft with statistics on the final ply
  recursive subroutine perft(p, depth, st)
    type(position_t), intent(in) :: p
    integer, intent(in) :: depth
    type(stats_t), intent(inout) :: st
    type(move_list_t) :: ml
    type(position_t) :: np
    integer :: i, m

    call gen_legal(p, ml)
    if (depth == 1) then
      do i = 1, ml%n
        m = ml%m(i)
        np = make_move(p, m)
        st%nodes = st%nodes + 1
        if (iand(m, MF_CAPTURE) /= 0) st%captures = st%captures + 1
        if (iand(m, MF_EP) /= 0) st%ep = st%ep + 1
        if (iand(m, MF_CASTLE) /= 0) st%castles = st%castles + 1
        if (iand(m, MF_PROMO_MASK) /= 0) st%promotions = st%promotions + 1
        if (is_attacked(np, king_square(np, np%side), p%side)) st%checks = st%checks + 1
      end do
    else
      do i = 1, ml%n
        np = make_move(p, ml%m(i))
        call perft(np, depth - 1, st)
      end do
    end if
  end subroutine perft

  ! Count leaf nodes after 'depth' half-moves (plies) with statistics
  subroutine run_perft(p, depth, st)
    type(position_t), intent(in) :: p
    integer, intent(in) :: depth
    type(stats_t), intent(out) :: st
    call init_engine()
    st = stats_t()
    if (depth <= 0) then
      st%nodes = 1_int64
    else
      call perft(p, depth, st)
    end if
  end subroutine run_perft

  !  UCI move string, e.g. "e2e4", "e7e8q"
  pure function move_to_uci(m) result(s)
    integer(int32), intent(in) :: m
    character(len=5) :: s
    integer :: from, to, promo
    from = iand(m, 63)
    to = iand(ishft(m, -6), 63)
    promo = iand(ishft(m, -12), 7)
    s(1:1) = achar(ichar('a') + modulo(from, 8))
    s(2:2) = achar(ichar('1') + from / 8)
    s(3:3) = achar(ichar('a') + modulo(to, 8))
    s(4:4) = achar(ichar('1') + to / 8)
    select case (promo)
    case (KNIGHT); s(5:5) = 'n'
    case (BISHOP); s(5:5) = 'b'
    case (ROOK);   s(5:5) = 'r'
    case (QUEEN);  s(5:5) = 'q'
    case default;  s(5:5) = ' '
    end select
  end function move_to_uci

    !  Serialise a position to a FEN string
  subroutine position_to_fen(p, fen)
    type(position_t), intent(in) :: p
    character(len=:), allocatable, intent(out) :: fen
    character(len=*), parameter :: PCHARS = 'PNBRQKpnbrqk'
    character(len=:), allocatable :: s
    character(len=8) :: num
    integer :: r, f, k, pc, empty
    integer(int64) :: bit

    s = ''
    do r = 7, 0, -1
      empty = 0
      do f = 0, 7
        bit = sqbit(8*r + f)
        pc = 0
        do k = 1, 12
          if (iand(p%bb(k), bit) /= 0_int64) then
            pc = k
            exit
          end if
        end do
        if (pc == 0) then
          empty = empty + 1
        else
          if (empty > 0) then
            write (num, '(i0)') empty
            s = s//trim(num)
            empty = 0
          end if
          s = s//PCHARS(pc:pc)
        end if
      end do
      if (empty > 0) then
        write (num, '(i0)') empty
        s = s//trim(num)
      end if
      if (r > 0) s = s//'/'
    end do

    if (p%side == WHITE) then
      s = s//' w '
    else
      s = s//' b '
    end if

    if (p%castling == 0) then
      s = s//'-'
    else
      if (iand(p%castling, CR_WK) /= 0) s = s//'K'
      if (iand(p%castling, CR_WQ) /= 0) s = s//'Q'
      if (iand(p%castling, CR_BK) /= 0) s = s//'k'
      if (iand(p%castling, CR_BQ) /= 0) s = s//'q'
    end if

    if (p%ep_sq < 0) then
      s = s//' - '
    else
      s = s//' '//achar(ichar('a') + modulo(p%ep_sq, 8)) &
                //achar(ichar('1') + p%ep_sq/8)//' '
    end if

    write (num, '(i0)') p%halfmove
    s = s//trim(num)//' '
    write (num, '(i0)') p%fullmove
    s = s//trim(num)
    fen = s
  end subroutine position_to_fen

  !  C interface helpers
  function c_to_f(cs) result(fs)
    character(kind=c_char), dimension(*), intent(in) :: cs
    character(len=:), allocatable :: fs
    integer :: n, i
    n = 0
    do while (n < 4096)
      if (cs(n + 1) == c_null_char) exit
      n = n + 1
    end do
    allocate (character(len=n) :: fs)
    do i = 1, n
      fs(i:i) = cs(i)
    end do
  end function c_to_f

  ! Initialise lookup tables. Call once before anything else (optional: the
  ! other entry points initialise on demand).
  subroutine chess_init() bind(C, name="chess_init")
    call init_engine()
  end subroutine chess_init

  ! Perft. counts(6) receives: nodes, captures, en passant, castles,
  ! promotions, checks. Returns 0 on success, -1 on invalid FEN.
  integer(c_int) function chess_perft(fen, depth, counts) bind(C, name="chess_perft")
    character(kind=c_char), dimension(*), intent(in) :: fen
    integer(c_int), value, intent(in) :: depth
    integer(c_int64_t), dimension(6), intent(out) :: counts
    type(position_t) :: p
    type(stats_t) :: st
    logical :: ok

    counts = 0_c_int64_t
    call parse_fen(c_to_f(fen), p, ok)
    if (.not. ok) then
      chess_perft = -1_c_int
      return
    end if
    call run_perft(p, int(depth), st)
    counts = [st%nodes, st%captures, st%ep, st%castles, st%promotions, st%checks]
    chess_perft = 0_c_int
  end function chess_perft

  ! Legal moves for a FEN. Writes up to max_moves encoded moves into 'moves'
  ! and returns the number of legal moves (or -1 on invalid FEN).
  integer(c_int) function chess_legal_moves(fen, moves, max_moves) bind(C, name="chess_legal_moves")
    character(kind=c_char), dimension(*), intent(in) :: fen
    integer(c_int32_t), dimension(*), intent(out) :: moves
    integer(c_int), value, intent(in) :: max_moves
    type(position_t) :: p
    type(move_list_t) :: ml
    logical :: ok
    integer :: i

    call init_engine()
    call parse_fen(c_to_f(fen), p, ok)
    if (.not. ok) then
      chess_legal_moves = -1_c_int
      return
    end if
    call gen_legal(p, ml)
    do i = 1, min(ml%n, int(max_moves))
      moves(i) = ml%m(i)
    end do
    chess_legal_moves = int(ml%n, c_int)
  end function chess_legal_moves

  ! Writes the UCI string of an encoded move into buf (NUL-terminated, >= 6 bytes)
  subroutine chess_move_uci(move, buf) bind(C, name="chess_move_uci")
    integer(c_int32_t), value, intent(in) :: move
    character(kind=c_char), dimension(*), intent(out) :: buf
    character(len=5) :: s
    integer :: i, n
    s = move_to_uci(move)
    n = len_trim(s)
    do i = 1, n
      buf(i) = s(i:i)
    end do
    buf(n + 1) = c_null_char
  end subroutine chess_move_uci

  ! Play a legal move (encoded as returned by chess_legal_moves) and write the
  ! resulting FEN into out_fen (NUL-terminated, capacity out_len).
  ! Returns 0 on success, -1 invalid FEN, -2 move not legal, -3 buffer too small.
  integer(c_int) function chess_make_move(fen, move, out_fen, out_len) bind(C, name="chess_make_move")
    character(kind=c_char), dimension(*), intent(in) :: fen
    integer(c_int32_t), value, intent(in) :: move
    character(kind=c_char), dimension(*), intent(out) :: out_fen
    integer(c_int), value, intent(in) :: out_len
    type(position_t) :: p, np
    type(move_list_t) :: ml
    character(len=:), allocatable :: s
    logical :: ok, found
    integer :: i, n

    call init_engine()
    call parse_fen(c_to_f(fen), p, ok)
    if (.not. ok) then
      chess_make_move = -1_c_int
      return
    end if

    call gen_legal(p, ml)
    found = .false.
    do i = 1, ml%n
      if (ml%m(i) == int(move, int32)) then
        found = .true.
        exit
      end if
    end do
    if (.not. found) then
      chess_make_move = -2_c_int
      return
    end if

    np = make_move(p, int(move, int32))
    call position_to_fen(np, s)
    n = len(s)
    if (n + 1 > int(out_len)) then
      chess_make_move = -3_c_int
      return
    end if
    do i = 1, n
      out_fen(i) = s(i:i)
    end do
    out_fen(n + 1) = c_null_char
    chess_make_move = 0_c_int
  end function chess_make_move

  ! 1 if the side to move is in check, 0 if not, -1 on invalid FEN.
  integer(c_int) function chess_in_check(fen) bind(C, name="chess_in_check")
    character(kind=c_char), dimension(*), intent(in) :: fen
    type(position_t) :: p
    logical :: ok

    call init_engine()
    call parse_fen(c_to_f(fen), p, ok)
    if (.not. ok .or. p%bb(6*p%side + KING) == 0_int64) then
      chess_in_check = -1_c_int
      return
    end if
    if (is_attacked(p, king_square(p, p%side), 1 - p%side)) then
      chess_in_check = 1_c_int
    else
      chess_in_check = 0_c_int
    end if
  end function chess_in_check

end module chess_engine