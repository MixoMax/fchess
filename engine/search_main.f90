! =============================================================================
!  search_main.f90
!  Command-line search driver for the Fortran 2008 chess engine
! =============================================================================
program search_main
  use chess_engine
  use chess_search_mod
  implicit none
  integer :: nargs, i, depth, ios
  character(len=1024) :: arg, fen_buf
  type(position_t) :: p
  type(search_info_t) :: info
  logical :: ok
  real :: t0, t1, elapsed, nps

  nargs = command_argument_count()
  if (nargs < 1) then
    write (*, '(a)') 'usage: search "<FEN>" <depth>   |   search <depth>  (start position)'
    stop 1
  end if

  call get_command_argument(nargs, arg)
  read (arg, *, iostat=ios) depth
  if (ios /= 0 .or. depth < 1) then
    write (*, '(a)') 'error: last argument must be a positive search depth'
    stop 1
  end if

  if (nargs == 1) then
    fen_buf = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'
  else
    fen_buf = ''
    do i = 1, nargs - 1
      call get_command_argument(i, arg)
      fen_buf = trim(adjustl(trim(fen_buf)//' '//trim(arg)))
    end do
  end if

  call init_search()
  call parse_fen(trim(fen_buf), p, ok)
  if (.not. ok) then
    write (*, '(a)') 'error: invalid FEN: '//trim(fen_buf)
    stop 1
  end if

  call cpu_time(t0)
  call search_position(p, depth, info)
  call cpu_time(t1)
  elapsed = max(t1 - t0, 1.0e-6)
  nps = real(info%nodes) / elapsed

  write (*, '(a,a)')       'FEN:         ', trim(fen_buf)
  write (*, '(a,i0)')      'Depth:       ', info%depth
  if (info%best_move /= 0) then
    write (*, '(a,a)')     'Best move:   ', trim(move_to_uci(info%best_move))
  else
    write (*, '(a)')       'Best move:   (none)'
  end if
  write (*, '(a,i0,a)')    'Score:       ', info%score, ' cp'
  write (*, '(a,i0)')      'Nodes:       ', info%nodes
  write (*, '(a,f10.3,a)') 'Time:        ', t1 - t0, ' s (CPU)'
  write (*, '(a,f10.1)')   'Nodes/s:     ', nps

end program search_main