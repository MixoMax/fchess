program perft_main
  use chess_engine
  implicit none
  integer :: nargs, i, depth, ios
  character(len=1024) :: arg, fen_buf
  type(position_t) :: p
  type(stats_t) :: st
  logical :: ok
  real :: t0, t1, nps, scaled_nps
  character(len=16) :: unit_prefix

  nargs = command_argument_count()
  if (nargs < 1) then
    write (*, '(a)') 'usage: perft "<FEN>" <plies>   |   perft <plies>  (start position)'
    stop 1
  end if

  call get_command_argument(nargs, arg)
  read (arg, *, iostat=ios) depth
  if (ios /= 0) then
    write (*, '(a)') 'error: last argument must be the number of plies'
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

  call init_engine()
  call parse_fen(trim(fen_buf), p, ok)
  if (.not. ok) then
    write (*, '(a)') 'error: invalid FEN: '//trim(fen_buf)
    stop 1
  end if

  call cpu_time(t0)
  call run_perft(p, depth, st)
  call cpu_time(t1)

  write (*, '(a,a)')   'FEN:         ', trim(fen_buf)
  write (*, '(a,a)')   'Plies:       ', trim(format_commas(int(depth, 8)))
  write (*, '(a,a)')   'Nodes:       ', trim(format_commas(st%nodes))
  write (*, '(a,a)')   'Captures:    ', trim(format_commas(st%captures))
  write (*, '(a,a)')   'En passant:  ', trim(format_commas(st%ep))
  write (*, '(a,a)')   'Castles:     ', trim(format_commas(st%castles))
  write (*, '(a,a)')   'Promotions:  ', trim(format_commas(st%promotions))
  write (*, '(a,a)')   'Checks:      ', trim(format_commas(st%checks))
  write (*, '(a,f10.3,a)') 'Time:        ', t1 - t0, ' s (CPU)'

  nps = real(st%nodes) / (t1 - t0)
  if (nps < 1.0e3) then
    scaled_nps = nps
    unit_prefix = ''
  else if (nps < 1.0e6) then
    scaled_nps = nps / 1.0e3
    unit_prefix = 'Thousand'
  else if (nps < 1.0e9) then
    scaled_nps = nps / 1.0e6
    unit_prefix = 'Million'
  else
    scaled_nps = nps / 1.0e9
    unit_prefix = 'Billion'
  end if

  write (*, '(a,f10.3,1x,a)') 'Nodes/s:     ', scaled_nps, trim(unit_prefix)

contains

  recursive function format_commas(n) result(res)
    integer(8), intent(in) :: n
    character(len=32) :: temp_str
    character(len=64) :: res
    integer :: length, num_commas, i, j, k

    if (n < 0) then
       res = '-' // trim(format_commas(-n))
       return
    end if

    write(temp_str, '(i0)') n
    temp_str = adjustl(temp_str)
    length = len_trim(temp_str)

    if (length <= 3) then
       res = trim(temp_str)
       return
    end if

    num_commas = (length - 1) / 3
    res = repeat(' ', 64)
    j = length + num_commas
    k = 0

    do i = length, 1, -1
       res(j:j) = temp_str(i:i)
       j = j - 1
       k = k + 1
       if (k == 3 .and. i > 1) then
          res(j:j) = ','
          j = j - 1
          k = 0
       end if
    end do
    res = trim(adjustl(res))
  end function format_commas

end program perft_main