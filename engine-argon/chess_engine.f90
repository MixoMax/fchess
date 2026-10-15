! ==============================================================================
! File: chess_engine.f90
! Standard: Fortran 2008
! Description: Bitboard Chess Move Generator, Perft Verifier, and C Shared Lib
! ==============================================================================
module chess_engine
    use, intrinsic :: iso_fortran_env, only: int64, int32
    use, intrinsic :: iso_c_binding, only: c_int, c_int64_t, c_char, c_null_char
    implicit none
    public

    ! Colors
    integer(int32), parameter :: WHITE = 0
    integer(int32), parameter :: BLACK = 1

    ! Piece Types (1 to 6)
    integer(int32), parameter :: PAWN   = 1
    integer(int32), parameter :: KNIGHT = 2
    integer(int32), parameter :: BISHOP = 3
    integer(int32), parameter :: ROOK   = 4
    integer(int32), parameter :: QUEEN  = 5
    integer(int32), parameter :: KING   = 6

    ! 12 Piece Bitboard Indices
    integer(int32), parameter :: W_PAWN   = 1,  W_KNIGHT = 2,  W_BISHOP = 3
    integer(int32), parameter :: W_ROOK   = 4,  W_QUEEN  = 5,  W_KING   = 6
    integer(int32), parameter :: B_PAWN   = 7,  B_KNIGHT = 8,  B_BISHOP = 9
    integer(int32), parameter :: B_ROOK   = 10, B_QUEEN  = 11, B_KING   = 12

    ! Castling Rights Bit Flags
    integer(int32), parameter :: CASTLE_WK = 1  ! 0001 (White Kingside)
    integer(int32), parameter :: CASTLE_WQ = 2  ! 0010 (White Queenside)
    integer(int32), parameter :: CASTLE_BK = 4  ! 0100 (Black Kingside)
    integer(int32), parameter :: CASTLE_BQ = 8  ! 1000 (Black Queenside)

    ! Move Encoding Flags (4-bit flags stored at bits 12..15)
    integer(int32), parameter :: FLAG_QUIET       = 0
    integer(int32), parameter :: FLAG_DOUBLE_PAWN = 1
    integer(int32), parameter :: FLAG_KING_CASTLE = 2
    integer(int32), parameter :: FLAG_QUEEN_CASTLE= 3
    integer(int32), parameter :: FLAG_CAPTURE     = 4
    integer(int32), parameter :: FLAG_EP_CAPTURE  = 5
    integer(int32), parameter :: FLAG_PROMO_N     = 8
    integer(int32), parameter :: FLAG_PROMO_B     = 9
    integer(int32), parameter :: FLAG_PROMO_R     = 10
    integer(int32), parameter :: FLAG_PROMO_Q     = 11
    integer(int32), parameter :: FLAG_PROMO_CAP_N = 12
    integer(int32), parameter :: FLAG_PROMO_CAP_B = 13
    integer(int32), parameter :: FLAG_PROMO_CAP_R = 14
    integer(int32), parameter :: FLAG_PROMO_CAP_Q = 15

    ! Ray Directions (0..7)
    integer(int32), parameter :: DIR_N  = 0, DIR_E  = 1, DIR_NW = 2, DIR_NE = 3
    integer(int32), parameter :: DIR_S  = 4, DIR_W  = 5, DIR_SW = 6, DIR_SE = 7

    ! File and Rank Bitboard Constants
    integer(int64), parameter :: FILE_A = int(z'0101010101010101', int64)
    integer(int64), parameter :: FILE_B = int(z'0202020202020202', int64)
    integer(int64), parameter :: FILE_G = int(z'4040404040404040', int64)
    integer(int64), parameter :: FILE_H = int(z'8080808080808080', int64)
    integer(int64), parameter :: NOT_FILE_A  = not(FILE_A)
    integer(int64), parameter :: NOT_FILE_H  = not(FILE_H)
    integer(int64), parameter :: NOT_FILE_AB = not(ior(FILE_A, FILE_B))
    integer(int64), parameter :: NOT_FILE_GH = not(ior(FILE_G, FILE_H))

    integer(int64), parameter :: RANK_1 = int(z'00000000000000FF', int64)
    integer(int64), parameter :: RANK_2 = int(z'000000000000FF00', int64)
    integer(int64), parameter :: RANK_3 = int(z'0000000000FF0000', int64)
    integer(int64), parameter :: RANK_6 = int(z'0000FF0000000000', int64)
    integer(int64), parameter :: RANK_7 = int(z'00FF000000000000', int64)
    integer(int64), parameter :: RANK_8 = int(z'FF00000000000000', int64)

    ! Precomputed Attack Tables
    logical, save :: engine_initialized = .false.
    integer(int64), save :: pawn_attacks(0:1, 0:63)
    integer(int64), save :: knight_attacks(0:63)
    integer(int64), save :: king_attacks(0:63)
    integer(int64), save :: ray_attacks(0:7, 0:63)
    integer(int32), save :: castling_mask(0:63)

    ! Board State Representation
    type :: board_t
        integer(int64) :: pieces(12)  = 0_int64
        integer(int64) :: occ(0:2)    = 0_int64 ! 0: White, 1: Black, 2: Both
        integer(int32) :: mailbox(0:63) = 0     ! 0 if empty, else 1..12
        integer(int32) :: side_to_move  = WHITE
        integer(int32) :: castling      = 0
        integer(int32) :: ep_sq         = -1
        integer(int32) :: halfmove      = 0
        integer(int32) :: fullmove      = 1
    end type board_t

    ! Move List Structure
    type :: move_list_t
        integer(int32) :: moves(256)
        integer(int32) :: count = 0
    end type move_list_t

    ! C-Interoperable Perft Result Struct
    type, bind(C) :: c_perft_result_t
        integer(c_int64_t) :: nodes
        integer(c_int64_t) :: captures
        integer(c_int64_t) :: ep_captures
        integer(c_int64_t) :: castles
        integer(c_int64_t) :: promotions
        integer(c_int64_t) :: checks
        integer(c_int64_t) :: checkmates
    end type c_perft_result_t

    ! C-Interoperable Move Struct for External Callers
    type, bind(C) :: c_move_t
        integer(c_int) :: from_sq
        integer(c_int) :: to_sq
        integer(c_int) :: promo_piece ! 0: None, 2: N, 3: B, 4: R, 5: Q
        integer(c_int) :: is_capture
        integer(c_int) :: is_castle
        integer(c_int) :: is_ep
        character(kind=c_char) :: uci(6) ! Null-terminated UCI string e.g. "e7e8q\0"
    end type c_move_t

contains

    ! ==========================================================================
    ! 1. Engine Initialization & Attack Table Precomputation
    ! ==========================================================================
    subroutine init_engine()
        integer(int32) :: sq, r, f, tr, tf
        integer(int64) :: b

        if (engine_initialized) return

        do sq = 0, 63
            b = ibset(0_int64, sq)
            r = sq / 8
            f = mod(sq, 8)

            ! Pawn attacks
            pawn_attacks(WHITE, sq) = ior(shiftl(iand(b, NOT_FILE_A), 7), &
                                          shiftl(iand(b, NOT_FILE_H), 9))
            pawn_attacks(BLACK, sq) = ior(shiftr(iand(b, NOT_FILE_H), 7), &
                                          shiftr(iand(b, NOT_FILE_A), 9))

            ! Knight attacks
            knight_attacks(sq) = ior( &
                ior(ior(shiftl(iand(b, NOT_FILE_A), 15), shiftl(iand(b, NOT_FILE_H), 17)), &
                    ior(shiftl(iand(b, NOT_FILE_AB), 6), shiftl(iand(b, NOT_FILE_GH), 10))), &
                ior(ior(shiftr(iand(b, NOT_FILE_H), 15), shiftr(iand(b, NOT_FILE_A), 17)), &
                    ior(shiftr(iand(b, NOT_FILE_GH), 6), shiftr(iand(b, NOT_FILE_AB), 10))) &
            )

            ! King attacks
            king_attacks(sq) = ior( &
                ior(ior(shiftl(b, 8), shiftr(b, 8)), &
                    ior(shiftr(iand(b, NOT_FILE_A), 1), shiftl(iand(b, NOT_FILE_H), 1))), &
                ior(ior(shiftl(iand(b, NOT_FILE_A), 7), shiftl(iand(b, NOT_FILE_H), 9)), &
                    ior(shiftr(iand(b, NOT_FILE_H), 7), shiftr(iand(b, NOT_FILE_A), 9))) &
            )

            ! Ray attacks in all 8 directions
            ray_attacks(:, sq) = 0_int64

            ! Positive Rays (Square index increases)
            do tr = r + 1, 7
                ray_attacks(DIR_N, sq) = ibset(ray_attacks(DIR_N, sq), tr * 8 + f)
            end do
            do tf = f + 1, 7
                ray_attacks(DIR_E, sq) = ibset(ray_attacks(DIR_E, sq), r * 8 + tf)
            end do
            tr = r + 1; tf = f - 1
            do while (tr <= 7 .and. tf >= 0)
                ray_attacks(DIR_NW, sq) = ibset(ray_attacks(DIR_NW, sq), tr * 8 + tf)
                tr = tr + 1; tf = tf - 1
            end do
            tr = r + 1; tf = f + 1
            do while (tr <= 7 .and. tf <= 7)
                ray_attacks(DIR_NE, sq) = ibset(ray_attacks(DIR_NE, sq), tr * 8 + tf)
                tr = tr + 1; tf = tf + 1
            end do

            ! Negative Rays (Square index decreases)
            do tr = r - 1, 0, -1
                ray_attacks(DIR_S, sq) = ibset(ray_attacks(DIR_S, sq), tr * 8 + f)
            end do
            do tf = f - 1, 0, -1
                ray_attacks(DIR_W, sq) = ibset(ray_attacks(DIR_W, sq), r * 8 + tf)
            end do
            tr = r - 1; tf = f - 1
            do while (tr >= 0 .and. tf >= 0)
                ray_attacks(DIR_SW, sq) = ibset(ray_attacks(DIR_SW, sq), tr * 8 + tf)
                tr = tr - 1; tf = tf - 1
            end do
            tr = r - 1; tf = f + 1
            do while (tr >= 0 .and. tf <= 7)
                ray_attacks(DIR_SE, sq) = ibset(ray_attacks(DIR_SE, sq), tr * 8 + tf)
                tr = tr - 1; tf = tf + 1
            end do
        end do

        ! Castling rights retention masks when a move touches `from_sq` or `to_sq`
        castling_mask = 15
        castling_mask(0)  = 15 - CASTLE_WQ ! a1
        castling_mask(4)  = 15 - (CASTLE_WK + CASTLE_WQ) ! e1
        castling_mask(7)  = 15 - CASTLE_WK ! h1
        castling_mask(56) = 15 - CASTLE_BQ ! a8
        castling_mask(60) = 15 - (CASTLE_BK + CASTLE_BQ) ! e8
        castling_mask(63) = 15 - CASTLE_BK ! h8

        engine_initialized = .true.
    end subroutine init_engine

    ! ==========================================================================
    ! 2. Sliding Piece Attacks (Ray Bitscan Method)
    ! ==========================================================================
    pure function get_PositiveRay_attacks(dir, sq, occ) result(attacks)
        integer(int32), intent(in) :: dir, sq
        integer(int64), intent(in) :: occ
        integer(int64) :: attacks, blockers
        integer(int32) :: first_blocker

        attacks = ray_attacks(dir, sq)
        blockers = iand(attacks, occ)
        if (blockers /= 0_int64) then
            first_blocker = trailz(blockers)
            attacks = ieor(attacks, ray_attacks(dir, first_blocker))
        end if
    end function get_PositiveRay_attacks

    pure function get_NegativeRay_attacks(dir, sq, occ) result(attacks)
        integer(int32), intent(in) :: dir, sq
        integer(int64), intent(in) :: occ
        integer(int64) :: attacks, blockers
        integer(int32) :: first_blocker

        attacks = ray_attacks(dir, sq)
        blockers = iand(attacks, occ)
        if (blockers /= 0_int64) then
            first_blocker = 63 - leadz(blockers)
            attacks = ieor(attacks, ray_attacks(dir, first_blocker))
        end if
    end function get_NegativeRay_attacks

    pure function get_rook_attacks(sq, occ) result(attacks)
        integer(int32), intent(in) :: sq
        integer(int64), intent(in) :: occ
        integer(int64) :: attacks

        attacks = ior( &
            ior(get_PositiveRay_attacks(DIR_N, sq, occ), get_PositiveRay_attacks(DIR_E, sq, occ)), &
            ior(get_NegativeRay_attacks(DIR_S, sq, occ), get_NegativeRay_attacks(DIR_W, sq, occ))  &
        )
    end function get_rook_attacks

    pure function get_bishop_attacks(sq, occ) result(attacks)
        integer(int32), intent(in) :: sq
        integer(int64), intent(in) :: occ
        integer(int64) :: attacks

        attacks = ior( &
            ior(get_PositiveRay_attacks(DIR_NW, sq, occ), get_PositiveRay_attacks(DIR_NE, sq, occ)), &
            ior(get_NegativeRay_attacks(DIR_SW, sq, occ), get_NegativeRay_attacks(DIR_SE, sq, occ))  &
        )
    end function get_bishop_attacks

    pure function get_queen_attacks(sq, occ) result(attacks)
        integer(int32), intent(in) :: sq
        integer(int64), intent(in) :: occ
        integer(int64) :: attacks

        attacks = ior(get_rook_attacks(sq, occ), get_bishop_attacks(sq, occ))
    end function get_queen_attacks

    ! ==========================================================================
    ! 3. Square Attack Detection
    ! ==========================================================================
    pure function is_square_attacked(b, sq, by_side) result(attacked)
        type(board_t), intent(in) :: b
        integer(int32), intent(in) :: sq, by_side
        logical :: attacked
        integer(int32) :: offset
        integer(int64) :: occ, rooks_queens, bishops_queens

        offset = by_side * 6

        ! 1. Attacked by Pawns (use reverse pawn attack lookup from target square)
        if (iand(pawn_attacks(1 - by_side, sq), b%pieces(offset + PAWN)) /= 0_int64) then
            attacked = .true.
            return
        end if

        ! 2. Attacked by Knights
        if (iand(knight_attacks(sq), b%pieces(offset + KNIGHT)) /= 0_int64) then
            attacked = .true.
            return
        end if

        ! 3. Attacked by King
        if (iand(king_attacks(sq), b%pieces(offset + KING)) /= 0_int64) then
            attacked = .true.
            return
        end if

        occ = b%occ(2)

        ! 4. Attacked by Bishops or Queens
        bishops_queens = ior(b%pieces(offset + BISHOP), b%pieces(offset + QUEEN))
        if (bishops_queens /= 0_int64) then
            if (iand(get_bishop_attacks(sq, occ), bishops_queens) /= 0_int64) then
                attacked = .true.
                return
            end if
        end if

        ! 5. Attacked by Rooks or Queens
        rooks_queens = ior(b%pieces(offset + ROOK), b%pieces(offset + QUEEN))
        if (rooks_queens /= 0_int64) then
            if (iand(get_rook_attacks(sq, occ), rooks_queens) /= 0_int64) then
                attacked = .true.
                return
            end if
        end if

        attacked = .false.
    end function is_square_attacked

    ! ==========================================================================
    ! 4. Move Encoding & Decoding Helpers
    ! ==========================================================================
    pure function encode_move(from_sq, to_sq, flags, piece, captured) result(m)
        integer(int32), intent(in) :: from_sq, to_sq, flags, piece, captured
        integer(int32) :: m
        m = ior(from_sq, ior(shiftl(to_sq, 6), &
            ior(shiftl(flags, 12), ior(shiftl(piece, 16), shiftl(captured, 20)))))
    end function encode_move

    pure function move_from(m) result(sq)
        integer(int32), intent(in) :: m
        integer(int32) :: sq
        sq = iand(m, 63)
    end function move_from

    pure function move_to(m) result(sq)
        integer(int32), intent(in) :: m
        integer(int32) :: sq
        sq = iand(shiftr(m, 6), 63)
    end function move_to

    pure function move_flags(m) result(fl)
        integer(int32), intent(in) :: m
        integer(int32) :: fl
        fl = iand(shiftr(m, 12), 15)
    end function move_flags

    pure function move_piece(m) result(p)
        integer(int32), intent(in) :: m
        integer(int32) :: p
        p = iand(shiftr(m, 16), 15)
    end function move_piece

    pure function move_captured(m) result(c)
        integer(int32), intent(in) :: m
        integer(int32) :: c
        c = iand(shiftr(m, 20), 15)
    end function move_captured

    pure subroutine add_move(list, m)
        type(move_list_t), intent(inout) :: list
        integer(int32), intent(in) :: m
        list%count = list%count + 1
        list%moves(list%count) = m
    end subroutine add_move

    ! ==========================================================================
    ! 5. Pseudo-Legal Bitboard Move Generator
    ! ==========================================================================
    pure subroutine generate_pseudo_legal_moves(b, list)
        type(board_t), intent(in) :: b
        type(move_list_t), intent(out) :: list

        integer(int32) :: us, them, offset, p_idx, from_sq, to_sq, cap_p
        integer(int64) :: our_occ, their_occ, all_occ, empty_occ
        integer(int64) :: bb, attacks, quiet_targets, cap_targets
        integer(int64) :: pawns, promo_pawns, non_promo_pawns
        integer(int64) :: single_push, double_push

        list%count = 0
        us = b%side_to_move
        them = 1 - us
        offset = us * 6

        our_occ   = b%occ(us)
        their_occ = b%occ(them)
        all_occ   = b%occ(2)
        empty_occ = not(all_occ)

        ! ----------------------------------------------------------------------
        ! A. PAWN MOVES
        ! ----------------------------------------------------------------------
        p_idx = offset + PAWN
        pawns = b%pieces(p_idx)

        if (us == WHITE) then
            promo_pawns     = iand(pawns, RANK_7)
            non_promo_pawns = iand(pawns, not(RANK_7))

            ! 1. Quiet pushes (non-promoting)
            single_push = iand(shiftl(non_promo_pawns, 8), empty_occ)
            double_push = iand(shiftl(iand(single_push, RANK_3), 8), empty_occ)

            bb = single_push
            do while (bb /= 0_int64)
                to_sq = trailz(bb)
                call add_move(list, encode_move(to_sq - 8, to_sq, FLAG_QUIET, p_idx, 0))
                bb = iand(bb, bb - 1_int64)
            end do

            bb = double_push
            do while (bb /= 0_int64)
                to_sq = trailz(bb)
                call add_move(list, encode_move(to_sq - 16, to_sq, FLAG_DOUBLE_PAWN, p_idx, 0))
                bb = iand(bb, bb - 1_int64)
            end do

            ! 2. Non-promoting captures
            bb = non_promo_pawns
            do while (bb /= 0_int64)
                from_sq = trailz(bb)
                attacks = iand(pawn_attacks(WHITE, from_sq), their_occ)
                do while (attacks /= 0_int64)
                    to_sq = trailz(attacks)
                    cap_p = b%mailbox(to_sq)
                    call add_move(list, encode_move(from_sq, to_sq, FLAG_CAPTURE, p_idx, cap_p))
                    attacks = iand(attacks, attacks - 1_int64)
                end do
                ! En Passant
                if (b%ep_sq /= -1) then
                    if (btest(pawn_attacks(WHITE, from_sq), b%ep_sq)) then
                        call add_move(list, encode_move(from_sq, b%ep_sq, FLAG_EP_CAPTURE, p_idx, B_PAWN))
                    end if
                end if
                bb = iand(bb, bb - 1_int64)
            end do

            ! 3. Promotions (Quiet & Captures)
            if (promo_pawns /= 0_int64) then
                bb = promo_pawns
                do while (bb /= 0_int64)
                    from_sq = trailz(bb)
                    to_sq = from_sq + 8
                    if (.not. btest(all_occ, to_sq)) then
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_Q, p_idx, 0))
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_R, p_idx, 0))
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_B, p_idx, 0))
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_N, p_idx, 0))
                    end if
                    attacks = iand(pawn_attacks(WHITE, from_sq), their_occ)
                    do while (attacks /= 0_int64)
                        to_sq = trailz(attacks)
                        cap_p = b%mailbox(to_sq)
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_CAP_Q, p_idx, cap_p))
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_CAP_R, p_idx, cap_p))
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_CAP_B, p_idx, cap_p))
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_CAP_N, p_idx, cap_p))
                        attacks = iand(attacks, attacks - 1_int64)
                    end do
                    bb = iand(bb, bb - 1_int64)
                end do
            end if

        else
            ! BLACK PAWNS
            promo_pawns     = iand(pawns, RANK_2)
            non_promo_pawns = iand(pawns, not(RANK_2))

            ! 1. Quiet pushes (non-promoting)
            single_push = iand(shiftr(non_promo_pawns, 8), empty_occ)
            double_push = iand(shiftr(iand(single_push, RANK_6), 8), empty_occ)

            bb = single_push
            do while (bb /= 0_int64)
                to_sq = trailz(bb)
                call add_move(list, encode_move(to_sq + 8, to_sq, FLAG_QUIET, p_idx, 0))
                bb = iand(bb, bb - 1_int64)
            end do

            bb = double_push
            do while (bb /= 0_int64)
                to_sq = trailz(bb)
                call add_move(list, encode_move(to_sq + 16, to_sq, FLAG_DOUBLE_PAWN, p_idx, 0))
                bb = iand(bb, bb - 1_int64)
            end do

            ! 2. Non-promoting captures
            bb = non_promo_pawns
            do while (bb /= 0_int64)
                from_sq = trailz(bb)
                attacks = iand(pawn_attacks(BLACK, from_sq), their_occ)
                do while (attacks /= 0_int64)
                    to_sq = trailz(attacks)
                    cap_p = b%mailbox(to_sq)
                    call add_move(list, encode_move(from_sq, to_sq, FLAG_CAPTURE, p_idx, cap_p))
                    attacks = iand(attacks, attacks - 1_int64)
                end do
                ! En Passant
                if (b%ep_sq /= -1) then
                    if (btest(pawn_attacks(BLACK, from_sq), b%ep_sq)) then
                        call add_move(list, encode_move(from_sq, b%ep_sq, FLAG_EP_CAPTURE, p_idx, W_PAWN))
                    end if
                end if
                bb = iand(bb, bb - 1_int64)
            end do

            ! 3. Promotions (Quiet & Captures)
            if (promo_pawns /= 0_int64) then
                bb = promo_pawns
                do while (bb /= 0_int64)
                    from_sq = trailz(bb)
                    to_sq = from_sq - 8
                    if (.not. btest(all_occ, to_sq)) then
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_Q, p_idx, 0))
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_R, p_idx, 0))
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_B, p_idx, 0))
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_N, p_idx, 0))
                    end if
                    attacks = iand(pawn_attacks(BLACK, from_sq), their_occ)
                    do while (attacks /= 0_int64)
                        to_sq = trailz(attacks)
                        cap_p = b%mailbox(to_sq)
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_CAP_Q, p_idx, cap_p))
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_CAP_R, p_idx, cap_p))
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_CAP_B, p_idx, cap_p))
                        call add_move(list, encode_move(from_sq, to_sq, FLAG_PROMO_CAP_N, p_idx, cap_p))
                        attacks = iand(attacks, attacks - 1_int64)
                    end do
                    bb = iand(bb, bb - 1_int64)
                end do
            end if
        end if

        ! ----------------------------------------------------------------------
        ! B. KNIGHT MOVES
        ! ----------------------------------------------------------------------
        p_idx = offset + KNIGHT
        bb = b%pieces(p_idx)
        do while (bb /= 0_int64)
            from_sq = trailz(bb)
            attacks = knight_attacks(from_sq)
            quiet_targets = iand(attacks, empty_occ)
            cap_targets   = iand(attacks, their_occ)

            do while (quiet_targets /= 0_int64)
                to_sq = trailz(quiet_targets)
                call add_move(list, encode_move(from_sq, to_sq, FLAG_QUIET, p_idx, 0))
                quiet_targets = iand(quiet_targets, quiet_targets - 1_int64)
            end do

            do while (cap_targets /= 0_int64)
                to_sq = trailz(cap_targets)
                call add_move(list, encode_move(from_sq, to_sq, FLAG_CAPTURE, p_idx, b%mailbox(to_sq)))
                cap_targets = iand(cap_targets, cap_targets - 1_int64)
            end do

            bb = iand(bb, bb - 1_int64)
        end do

        ! ----------------------------------------------------------------------
        ! C. BISHOP MOVES
        ! ----------------------------------------------------------------------
        p_idx = offset + BISHOP
        bb = b%pieces(p_idx)
        do while (bb /= 0_int64)
            from_sq = trailz(bb)
            attacks = get_bishop_attacks(from_sq, all_occ)
            quiet_targets = iand(attacks, empty_occ)
            cap_targets   = iand(attacks, their_occ)

            do while (quiet_targets /= 0_int64)
                to_sq = trailz(quiet_targets)
                call add_move(list, encode_move(from_sq, to_sq, FLAG_QUIET, p_idx, 0))
                quiet_targets = iand(quiet_targets, quiet_targets - 1_int64)
            end do

            do while (cap_targets /= 0_int64)
                to_sq = trailz(cap_targets)
                call add_move(list, encode_move(from_sq, to_sq, FLAG_CAPTURE, p_idx, b%mailbox(to_sq)))
                cap_targets = iand(cap_targets, cap_targets - 1_int64)
            end do

            bb = iand(bb, bb - 1_int64)
        end do

        ! ----------------------------------------------------------------------
        ! D. ROOK MOVES
        ! ----------------------------------------------------------------------
        p_idx = offset + ROOK
        bb = b%pieces(p_idx)
        do while (bb /= 0_int64)
            from_sq = trailz(bb)
            attacks = get_rook_attacks(from_sq, all_occ)
            quiet_targets = iand(attacks, empty_occ)
            cap_targets   = iand(attacks, their_occ)

            do while (quiet_targets /= 0_int64)
                to_sq = trailz(quiet_targets)
                call add_move(list, encode_move(from_sq, to_sq, FLAG_QUIET, p_idx, 0))
                quiet_targets = iand(quiet_targets, quiet_targets - 1_int64)
            end do

            do while (cap_targets /= 0_int64)
                to_sq = trailz(cap_targets)
                call add_move(list, encode_move(from_sq, to_sq, FLAG_CAPTURE, p_idx, b%mailbox(to_sq)))
                cap_targets = iand(cap_targets, cap_targets - 1_int64)
            end do

            bb = iand(bb, bb - 1_int64)
        end do

        ! ----------------------------------------------------------------------
        ! E. QUEEN MOVES
        ! ----------------------------------------------------------------------
        p_idx = offset + QUEEN
        bb = b%pieces(p_idx)
        do while (bb /= 0_int64)
            from_sq = trailz(bb)
            attacks = get_queen_attacks(from_sq, all_occ)
            quiet_targets = iand(attacks, empty_occ)
            cap_targets   = iand(attacks, their_occ)

            do while (quiet_targets /= 0_int64)
                to_sq = trailz(quiet_targets)
                call add_move(list, encode_move(from_sq, to_sq, FLAG_QUIET, p_idx, 0))
                quiet_targets = iand(quiet_targets, quiet_targets - 1_int64)
            end do

            do while (cap_targets /= 0_int64)
                to_sq = trailz(cap_targets)
                call add_move(list, encode_move(from_sq, to_sq, FLAG_CAPTURE, p_idx, b%mailbox(to_sq)))
                cap_targets = iand(cap_targets, cap_targets - 1_int64)
            end do

            bb = iand(bb, bb - 1_int64)
        end do

        ! ----------------------------------------------------------------------
        ! F. KING MOVES & CASTLING
        ! ----------------------------------------------------------------------
        p_idx = offset + KING
        bb = b%pieces(p_idx)
        if (bb /= 0_int64) then
            from_sq = trailz(bb)
            attacks = king_attacks(from_sq)
            quiet_targets = iand(attacks, empty_occ)
            cap_targets   = iand(attacks, their_occ)

            do while (quiet_targets /= 0_int64)
                to_sq = trailz(quiet_targets)
                call add_move(list, encode_move(from_sq, to_sq, FLAG_QUIET, p_idx, 0))
                quiet_targets = iand(quiet_targets, quiet_targets - 1_int64)
            end do

            do while (cap_targets /= 0_int64)
                to_sq = trailz(cap_targets)
                call add_move(list, encode_move(from_sq, to_sq, FLAG_CAPTURE, p_idx, b%mailbox(to_sq)))
                cap_targets = iand(cap_targets, cap_targets - 1_int64)
            end do

            ! Castling (Path occupancy & attack checks on king start and transit squares)
            if (us == WHITE .and. from_sq == 4) then
                ! White Kingside (e1 -> g1, squares f1=5, g1=6 must be empty)
                if (iand(b%castling, CASTLE_WK) /= 0) then
                    if (.not. btest(all_occ, 5) .and. .not. btest(all_occ, 6)) then
                        if (.not. is_square_attacked(b, 4, BLACK) .and. &
                            .not. is_square_attacked(b, 5, BLACK)) then
                            call add_move(list, encode_move(4, 6, FLAG_KING_CASTLE, W_KING, 0))
                        end if
                    end if
                end if
                ! White Queenside (e1 -> c1, squares b1=1, c1=2, d1=3 must be empty)
                if (iand(b%castling, CASTLE_WQ) /= 0) then
                    if (.not. btest(all_occ, 1) .and. .not. btest(all_occ, 2) .and. .not. btest(all_occ, 3)) then
                        if (.not. is_square_attacked(b, 4, BLACK) .and. &
                            .not. is_square_attacked(b, 3, BLACK)) then
                            call add_move(list, encode_move(4, 2, FLAG_QUEEN_CASTLE, W_KING, 0))
                        end if
                    end if
                end if
            else if (us == BLACK .and. from_sq == 60) then
                ! Black Kingside (e8 -> g8, squares f8=61, g8=62 must be empty)
                if (iand(b%castling, CASTLE_BK) /= 0) then
                    if (.not. btest(all_occ, 61) .and. .not. btest(all_occ, 62)) then
                        if (.not. is_square_attacked(b, 60, WHITE) .and. &
                            .not. is_square_attacked(b, 61, WHITE)) then
                            call add_move(list, encode_move(60, 62, FLAG_KING_CASTLE, B_KING, 0))
                        end if
                    end if
                end if
                ! Black Queenside (e8 -> c8, squares b8=57, c8=58, d8=59 must be empty)
                if (iand(b%castling, CASTLE_BQ) /= 0) then
                    if (.not. btest(all_occ, 57) .and. .not. btest(all_occ, 58) .and. .not. btest(all_occ, 59)) then
                        if (.not. is_square_attacked(b, 60, WHITE) .and. &
                            .not. is_square_attacked(b, 59, WHITE)) then
                            call add_move(list, encode_move(60, 58, FLAG_QUEEN_CASTLE, B_KING, 0))
                        end if
                    end if
                end if
            end if
        end if
    end subroutine generate_pseudo_legal_moves

    ! ==========================================================================
    ! 6. Make Move & Legal Move Filtering
    ! ==========================================================================
    pure subroutine make_move(b_in, m, b_out, is_legal)
        type(board_t), intent(in)  :: b_in
        integer(int32), intent(in) :: m
        type(board_t), intent(out) :: b_out
        logical, intent(out)       :: is_legal

        integer(int32) :: from_sq, to_sq, fl, p_idx, cap_p, us, them
        integer(int32) :: ep_cap_sq, promo_p, king_sq

        b_out = b_in
        us    = b_in%side_to_move
        them  = 1 - us

        from_sq = move_from(m)
        to_sq   = move_to(m)
        fl      = move_flags(m)
        p_idx   = move_piece(m)
        cap_p   = move_captured(m)

        ! Remove moving piece from source square
        b_out%pieces(p_idx) = ibclr(b_out%pieces(p_idx), from_sq)
        b_out%mailbox(from_sq) = 0

        ! Handle Captures
        if (fl == FLAG_CAPTURE .or. fl >= FLAG_PROMO_CAP_N) then
            b_out%pieces(cap_p) = ibclr(b_out%pieces(cap_p), to_sq)
        else if (fl == FLAG_EP_CAPTURE) then
            if (us == WHITE) then
                ep_cap_sq = to_sq - 8
            else
                ep_cap_sq = to_sq + 8
            end if
            b_out%pieces(cap_p) = ibclr(b_out%pieces(cap_p), ep_cap_sq)
            b_out%mailbox(ep_cap_sq) = 0
        end if

        ! Place piece on destination (handle Promotions vs Normal placement)
        if (fl >= FLAG_PROMO_N) then
            select case (iand(fl, 3))
            case (0); promo_p = us * 6 + KNIGHT
            case (1); promo_p = us * 6 + BISHOP
            case (2); promo_p = us * 6 + ROOK
            case (3); promo_p = us * 6 + QUEEN
            end select
            b_out%pieces(promo_p) = ibset(b_out%pieces(promo_p), to_sq)
            b_out%mailbox(to_sq)  = promo_p
        else
            b_out%pieces(p_idx)  = ibset(b_out%pieces(p_idx), to_sq)
            b_out%mailbox(to_sq) = p_idx
        end if

        ! Handle Rook Movement on Castling
        if (fl == FLAG_KING_CASTLE) then
            if (us == WHITE) then
                b_out%pieces(W_ROOK) = ibclr(b_out%pieces(W_ROOK), 7)
                b_out%pieces(W_ROOK) = ibset(b_out%pieces(W_ROOK), 5)
                b_out%mailbox(7) = 0
                b_out%mailbox(5) = W_ROOK
            else
                b_out%pieces(B_ROOK) = ibclr(b_out%pieces(B_ROOK), 63)
                b_out%pieces(B_ROOK) = ibset(b_out%pieces(B_ROOK), 61)
                b_out%mailbox(63) = 0
                b_out%mailbox(61) = B_ROOK
            end if
        else if (fl == FLAG_QUEEN_CASTLE) then
            if (us == WHITE) then
                b_out%pieces(W_ROOK) = ibclr(b_out%pieces(W_ROOK), 0)
                b_out%pieces(W_ROOK) = ibset(b_out%pieces(W_ROOK), 3)
                b_out%mailbox(0) = 0
                b_out%mailbox(3) = W_ROOK
            else
                b_out%pieces(B_ROOK) = ibclr(b_out%pieces(B_ROOK), 56)
                b_out%pieces(B_ROOK) = ibset(b_out%pieces(B_ROOK), 59)
                b_out%mailbox(56) = 0
                b_out%mailbox(59) = B_ROOK
            end if
        end if

        ! Update En Passant target square
        if (fl == FLAG_DOUBLE_PAWN) then
            if (us == WHITE) then
                b_out%ep_sq = from_sq + 8
            else
                b_out%ep_sq = from_sq - 8
            end if
        else
            b_out%ep_sq = -1
        end if

        ! Update Castling Rights
        b_out%castling = iand(b_out%castling, iand(castling_mask(from_sq), castling_mask(to_sq)))

        ! Recompute Occupancy Bitboards
        b_out%occ(WHITE) = ior(ior(ior(b_out%pieces(1), b_out%pieces(2)), &
                                   ior(b_out%pieces(3), b_out%pieces(4))), &
                               ior(b_out%pieces(5), b_out%pieces(6)))
        b_out%occ(BLACK) = ior(ior(ior(b_out%pieces(7), b_out%pieces(8)), &
                                   ior(b_out%pieces(9), b_out%pieces(10))), &
                               ior(b_out%pieces(11), b_out%pieces(12)))
        b_out%occ(2)     = ior(b_out%occ(WHITE), b_out%occ(BLACK))

        b_out%side_to_move = them

        ! Verify our King is not left in check
        king_sq = trailz(b_out%pieces(us * 6 + KING))
        is_legal = .not. is_square_attacked(b_out, king_sq, them)
    end subroutine make_move

    pure subroutine generate_legal_moves(b, legal_list)
        type(board_t), intent(in)      :: b
        type(move_list_t), intent(out) :: legal_list
        type(move_list_t) :: pseudo_list
        type(board_t)     :: next_b
        integer(int32)    :: i
        logical           :: is_legal

        call generate_pseudo_legal_moves(b, pseudo_list)
        legal_list%count = 0

        do i = 1, pseudo_list%count
            call make_move(b, pseudo_list%moves(i), next_b, is_legal)
            if (is_legal) then
                call add_move(legal_list, pseudo_list%moves(i))
            end if
        end do
    end subroutine generate_legal_moves

    pure function has_any_legal_move(b) result(has_move)
        type(board_t), intent(in) :: b
        logical :: has_move
        type(move_list_t) :: pseudo_list
        type(board_t)     :: next_b
        integer(int32)    :: i
        logical           :: is_legal

        call generate_pseudo_legal_moves(b, pseudo_list)
        do i = 1, pseudo_list%count
            call make_move(b, pseudo_list%moves(i), next_b, is_legal)
            if (is_legal) then
                has_move = .true.
                return
            end if
        end do
        has_move = .false.
    end function has_any_legal_move

    ! ==========================================================================
    ! 7. Recursive Perft with Detailed Statistics
    ! ==========================================================================
    recursive subroutine perft_search(b, depth, res)
        type(board_t), intent(in)               :: b
        integer(int32), intent(in)              :: depth
        type(c_perft_result_t), intent(inout)   :: res

        type(move_list_t) :: pseudo_list
        type(board_t)     :: next_b
        integer(int32)    :: i, m, fl, enemy_king_sq
        logical           :: is_legal

        if (depth == 0) then
            res%nodes = res%nodes + 1_c_int64_t
            return
        end if

        call generate_pseudo_legal_moves(b, pseudo_list)

        do i = 1, pseudo_list%count
            m = pseudo_list%moves(i)
            call make_move(b, m, next_b, is_legal)
            if (.not. is_legal) cycle

            if (depth == 1) then
                res%nodes = res%nodes + 1_c_int64_t
                fl = move_flags(m)

                ! Classify Captures
                if (fl == FLAG_CAPTURE .or. fl == FLAG_EP_CAPTURE .or. fl >= FLAG_PROMO_CAP_N) then
                    res%captures = res%captures + 1_c_int64_t
                end if

                ! Classify En Passant
                if (fl == FLAG_EP_CAPTURE) then
                    res%ep_captures = res%ep_captures + 1_c_int64_t
                end if

                ! Classify Castles
                if (fl == FLAG_KING_CASTLE .or. fl == FLAG_QUEEN_CASTLE) then
                    res%castles = res%castles + 1_c_int64_t
                end if

                ! Classify Promotions
                if (fl >= FLAG_PROMO_N) then
                    res%promotions = res%promotions + 1_c_int64_t
                end if

                ! Classify Checks & Checkmates delivered by this move
                enemy_king_sq = trailz(next_b%pieces(next_b%side_to_move * 6 + KING))
                if (is_square_attacked(next_b, enemy_king_sq, b%side_to_move)) then
                    res%checks = res%checks + 1_c_int64_t
                    if (.not. has_any_legal_move(next_b)) then
                        res%checkmates = res%checkmates + 1_c_int64_t
                    end if
                end if
            else
                call perft_search(next_b, depth - 1, res)
            end if
        end do
    end subroutine perft_search

    ! ==========================================================================
    ! 8. FEN Parser & UCI Move Formatter
    ! ==========================================================================
    subroutine parse_fen(fen_str, b)
        character(len=*), intent(in) :: fen_str
        type(board_t), intent(out)   :: b

        integer(int32) :: idx, length, rank, file, sq, p_idx, i
        character(len=1) :: ch
        character(len=256) :: clean_fen

        if (.not. engine_initialized) call init_engine()

        b%pieces       = 0_int64
        b%occ          = 0_int64
        b%mailbox      = 0
        b%side_to_move = WHITE
        b%castling     = 0
        b%ep_sq        = -1
        b%halfmove     = 0
        b%fullmove     = 1

        clean_fen = adjustl(fen_str)
        length = len_trim(clean_fen)
        idx = 1
        rank = 7
        file = 0

        ! 1. Piece placement field
        do while (idx <= length)
            ch = clean_fen(idx:idx)
            if (ch == ' ') exit
            if (ch == '/') then
                rank = rank - 1
                file = 0
            else if (ch >= '1' .and. ch <= '8') then
                file = file + (iachar(ch) - iachar('0'))
            else
                sq = rank * 8 + file
                p_idx = 0
                select case (ch)
                case ('P'); p_idx = W_PAWN
                case ('N'); p_idx = W_KNIGHT
                case ('B'); p_idx = W_BISHOP
                case ('R'); p_idx = W_ROOK
                case ('Q'); p_idx = W_QUEEN
                case ('K'); p_idx = W_KING
                case ('p'); p_idx = B_PAWN
                case ('n'); p_idx = B_KNIGHT
                case ('b'); p_idx = B_BISHOP
                case ('r'); p_idx = B_ROOK
                case ('q'); p_idx = B_QUEEN
                case ('k'); p_idx = B_KING
                end select
                if (p_idx > 0) then
                    b%pieces(p_idx) = ibset(b%pieces(p_idx), sq)
                    b%mailbox(sq)   = p_idx
                end if
                file = file + 1
            end if
            idx = idx + 1
        end do

        ! Skip spaces
        do while (idx <= length .and. clean_fen(idx:idx) == ' ')
            idx = idx + 1
        end do

        ! 2. Side to move
        if (idx <= length) then
            if (clean_fen(idx:idx) == 'b' .or. clean_fen(idx:idx) == 'B') then
                b%side_to_move = BLACK
            else
                b%side_to_move = WHITE
            end if
            idx = idx + 1
        end if

        do while (idx <= length .and. clean_fen(idx:idx) == ' ')
            idx = idx + 1
        end do

        ! 3. Castling rights
        do while (idx <= length)
            ch = clean_fen(idx:idx)
            if (ch == ' ') exit
            select case (ch)
            case ('K'); b%castling = ior(b%castling, CASTLE_WK)
            case ('Q'); b%castling = ior(b%castling, CASTLE_WQ)
            case ('k'); b%castling = ior(b%castling, CASTLE_BK)
            case ('q'); b%castling = ior(b%castling, CASTLE_BQ)
            end select
            idx = idx + 1
        end do

        do while (idx <= length .and. clean_fen(idx:idx) == ' ')
            idx = idx + 1
        end do

        ! 4. En Passant square
        if (idx <= length) then
            ch = clean_fen(idx:idx)
            if (ch /= '-') then
                file = iachar(ch) - iachar('a')
                idx = idx + 1
                if (idx <= length) then
                    rank = iachar(clean_fen(idx:idx)) - iachar('1')
                    b%ep_sq = rank * 8 + file
                    idx = idx + 1
                end if
            else
                idx = idx + 1
            end if
        end if

        ! Set occupancy bitboards
        do i = 1, 6
            b%occ(WHITE) = ior(b%occ(WHITE), b%pieces(i))
            b%occ(BLACK) = ior(b%occ(BLACK), b%pieces(i + 6))
        end do
        b%occ(2) = ior(b%occ(WHITE), b%occ(BLACK))
    end subroutine parse_fen

    pure subroutine move_to_uci(m, uci_str)
        integer(int32), intent(in)   :: m
        character(len=5), intent(out):: uci_str
        integer(int32) :: f_sq, t_sq, fl

        f_sq = move_from(m)
        t_sq = move_to(m)
        fl   = move_flags(m)

        uci_str(1:1) = achar(iachar('a') + mod(f_sq, 8))
        uci_str(2:2) = achar(iachar('1') + (f_sq / 8))
        uci_str(3:3) = achar(iachar('a') + mod(t_sq, 8))
        uci_str(4:4) = achar(iachar('1') + (t_sq / 8))
        uci_str(5:5) = ' '

        if (fl >= FLAG_PROMO_N) then
            select case (iand(fl, 3))
            case (0); uci_str(5:5) = 'n'
            case (1); uci_str(5:5) = 'b'
            case (2); uci_str(5:5) = 'r'
            case (3); uci_str(5:5) = 'q'
            end select
        end if
    end subroutine move_to_uci

    ! ==========================================================================
    ! 9. C-Interoperable Shared Library Interface (bind(C))
    ! ==========================================================================
    subroutine c_string_to_f_string(c_str, f_str)
        character(kind=c_char), intent(in) :: c_str(*)
        character(len=*), intent(out)      :: f_str
        integer :: i

        f_str = ' '
        do i = 1, len(f_str)
            if (c_str(i) == c_null_char) exit
            f_str(i:i) = c_str(i)
        end do
    end subroutine c_string_to_f_string

    subroutine c_init_engine() bind(C, name="c_init_engine")
        call init_engine()
    end subroutine c_init_engine

    subroutine c_perft_fen(fen_c, depth, res) bind(C, name="c_perft_fen")
        character(kind=c_char), intent(in)    :: fen_c(*)
        integer(c_int), value, intent(in)     :: depth
        type(c_perft_result_t), intent(out)   :: res

        character(len=256) :: fen_f
        type(board_t)      :: b

        call init_engine()
        call c_string_to_f_string(fen_c, fen_f)
        call parse_fen(fen_f, b)

        res%nodes       = 0_c_int64_t
        res%captures    = 0_c_int64_t
        res%ep_captures = 0_c_int64_t
        res%castles     = 0_c_int64_t
        res%promotions  = 0_c_int64_t
        res%checks      = 0_c_int64_t
        res%checkmates  = 0_c_int64_t

        call perft_search(b, int(depth, int32), res)
    end subroutine c_perft_fen

    function c_get_legal_moves_fen(fen_c, out_moves, max_moves) result(num_moves) &
            bind(C, name="c_get_legal_moves_fen")
        character(kind=c_char), intent(in) :: fen_c(*)
        integer(c_int), value, intent(in)  :: max_moves
        type(c_move_t), intent(out)        :: out_moves(max_moves)
        integer(c_int)                     :: num_moves

        character(len=256) :: fen_f
        character(len=5)   :: uci_f
        type(board_t)      :: b
        type(move_list_t)  :: legal_list
        integer(int32)     :: i, m, fl, limit

        call init_engine()
        call c_string_to_f_string(fen_c, fen_f)
        call parse_fen(fen_f, b)
        call generate_legal_moves(b, legal_list)

        limit = min(legal_list%count, int(max_moves, int32))
        do i = 1, limit
            m  = legal_list%moves(i)
            fl = move_flags(m)

            out_moves(i)%from_sq = move_from(m)
            out_moves(i)%to_sq   = move_to(m)

            if (fl >= FLAG_PROMO_N) then
                out_moves(i)%promo_piece = iand(fl, 3) + 2 ! 2=N, 3=B, 4=R, 5=Q
            else
                out_moves(i)%promo_piece = 0
            end if

            if (fl == FLAG_CAPTURE .or. fl == FLAG_EP_CAPTURE .or. fl >= FLAG_PROMO_CAP_N) then
                out_moves(i)%is_capture = 1
            else
                out_moves(i)%is_capture = 0
            end if

            if (fl == FLAG_KING_CASTLE .or. fl == FLAG_QUEEN_CASTLE) then
                out_moves(i)%is_castle = 1
            else
                out_moves(i)%is_castle = 0
            end if

            if (fl == FLAG_EP_CAPTURE) then
                out_moves(i)%is_ep = 1
            else
                out_moves(i)%is_ep = 0
            end if

            call move_to_uci(m, uci_f)
            out_moves(i)%uci(1) = uci_f(1:1)
            out_moves(i)%uci(2) = uci_f(2:2)
            out_moves(i)%uci(3) = uci_f(3:3)
            out_moves(i)%uci(4) = uci_f(4:4)
            if (uci_f(5:5) /= ' ') then
                out_moves(i)%uci(5) = uci_f(5:5)
                out_moves(i)%uci(6) = c_null_char
            else
                out_moves(i)%uci(5) = c_null_char
                out_moves(i)%uci(6) = c_null_char
            end if
        end do

        num_moves = int(legal_list%count, c_int)
    end function c_get_legal_moves_fen

end module chess_engine

! ==============================================================================
! Standalone CLI Verification Program
! Usage:
!   ./chess_engine "<FEN>" <depth>
!   ./chess_engine                 (Runs built-in verification test suite)
! ==============================================================================
program main
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use chess_engine
    implicit none

    integer :: argc
    character(len=256) :: fen_arg, depth_arg
    integer(int32) :: depth
    type(board_t) :: b
    type(c_perft_result_t) :: res
    integer(int64) :: t_start, t_end, t_rate
    real(real64) :: elapsed, nps

    call init_engine()
    argc = command_argument_count()

    if (argc >= 2) then
        call get_command_argument(1, fen_arg)
        call get_command_argument(2, depth_arg)
        read(depth_arg, *) depth

        call parse_fen(fen_arg, b)
        res = c_perft_result_t(0, 0, 0, 0, 0, 0, 0)

        call system_clock(t_start, t_rate)
        call perft_search(b, depth, res)
        call system_clock(t_end)

        elapsed = real(t_end - t_start, real64) / real(t_rate, real64)
        if (elapsed > 0.0_real64) then
            nps = real(res%nodes, real64) / elapsed
        else
            nps = 0.0_real64
        end if

        print '(A, A)',      "FEN        : ", trim(fen_arg)
        print '(A, I0)',     "Depth (N)  : ", depth
        print '(A, I0)',     "Nodes      : ", res%nodes
        print '(A, I0)',     "Captures   : ", res%captures
        print '(A, I0)',     "En Passant : ", res%ep_captures
        print '(A, I0)',     "Castles    : ", res%castles
        print '(A, I0)',     "Promotions : ", res%promotions
        print '(A, I0)',     "Checks     : ", res%checks
        print '(A, I0)',     "Checkmates : ", res%checkmates
        print '(A, F0.4, A, F0.2, A)', "Time       : ", elapsed, " s (", nps / 1.0e6_real64, " MNPS)"
    else
        print '(A)', "Running Standard CPW Perft Verification Suite..."
        print '(A)', "----------------------------------------------------------------------"
        call run_test("Startpos (Depth 4)", &
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 4, &
            197281_int64, 1576_int64, 0_int64, 0_int64, 0_int64, 469_int64)

        call run_test("Kiwipete (Depth 4)", &
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 4, &
            4085603_int64, 757163_int64, 1929_int64, 128013_int64, 15172_int64, 25523_int64)

        call run_test("Position 3 (Depth 5)", &
            "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 5, &
            674624_int64, 52051_int64, 1165_int64, 0_int64, 0_int64, 52950_int64)

        call run_test("Position 4 (Depth 4)", &
            "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 4, &
            422333_int64, 131393_int64, 0_int64, 7795_int64, 60032_int64, 15492_int64)
    end if

contains

    subroutine run_test(title, fen, d, exp_nodes, exp_caps, exp_ep, exp_castles, exp_promos, exp_checks)
        character(len=*), intent(in) :: title, fen
        integer(int32), intent(in)   :: d
        integer(int64), intent(in)   :: exp_nodes, exp_caps, exp_ep, exp_castles, exp_promos, exp_checks
        type(board_t) :: tb
        type(c_perft_result_t) :: r
        logical :: ok

        call parse_fen(fen, tb)
        r = c_perft_result_t(0, 0, 0, 0, 0, 0, 0)
        call perft_search(tb, d, r)

        ok = (r%nodes == exp_nodes) .and. (r%captures == exp_caps) .and. &
             (r%ep_captures == exp_ep) .and. (r%castles == exp_castles) .and. &
             (r%promotions == exp_promos) .and. (r%checks == exp_checks)

        print '(A, A, A)', "[", merge("PASS", "FAIL", ok), "] " // trim(title)
        print '(A, I10, A, I10, A, I8, A, I8, A, I8, A, I8)', &
            "  Nodes:", r%nodes, " | Caps:", r%captures, " | EP:", r%ep_captures, &
            " | Castles:", r%castles, " | Promos:", r%promotions, " | Checks:", r%checks
    end subroutine run_test

end program main