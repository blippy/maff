! test the maff routines
! throughout, 'res' is assumed to mean known results
program maff90

use maff
implicit none

print *, "test of maff"


!print *, "days 1900: ..."
!print *, "1900-01-01: ", days1900(1900,1,1)
!print *, "2014-12-27: ", days1900(2014,12,27)

call test_dates
call test_i2ymd
call test_sortd
call test_quantile

contains

  subroutine test_i2ymd
    !use maff
    integer:: i, res, y, m, d
    print *, 'test_i2ymd'
    do i = 1, 800
       call i2ymd(i, y, m, d)
       !call date_int(res, y, m, d)
       print *, 'foo', i, y, m, d, int_date(y, m, d)
    enddo
  end subroutine test_i2ymd
  
  subroutine test_dates
  use maff
  integer :: i, n
  integer :: y,m,d, ans, res
  print *, "test_dates"
  open(unit = 24, file = 'xlsm/dates.txt')
  read(unit=24, fmt = *) n
  print *, 'n=', n
  do i = 1, n
     read(unit=24, fmt = *) y, m, d, ans
     res = days1900(y, m, d)
     if(ans.ne.res) then
        print *, y , m, d, ans, res, ans - res
        stop
     endif
  enddo
  print *, "pass"
  
end subroutine test_dates

subroutine test_sortd
  use maff
  integer :: i, n
  double precision, allocatable :: arr(:), res(:) ! res is known output
  print *, "test_sortd"
  open(unit = 24, file = 'xlsm/sort.txt')
  read(unit=24, fmt = *) n
  !print *, 'n=', n
  allocate(arr(n), res(n))
  !do i = 1, n
  read(unit=24, fmt=*) arr, res
  close(24)

  call sortd(arr)
  do i =1,n
     if(arr(i).ne.res(i)) then
        print *, "fail"
        return
     endif
  enddo
  print *, "pass"
  
     
end subroutine test_sortd

subroutine test_quantile
  !use maff
  integer :: i, n
  double precision, allocatable :: arr(:), qs(:), res(:)
  double precision :: dummy, q
  print *, "test_quantile"
  open(unit = 24, file = 'xlsm/random1.txt')
  read(unit=24, fmt = *) n
  print *, 'n=', n
  allocate(arr(n))
  do i = 1, n
     read(unit=24, fmt=*) arr(i), dummy
  enddo
  close(24)

  call sortd(arr)
  !do i =1,n
  !   if(arr(i).ne.res(i)) then
  !      print *, "fail"
  !      return
  !   endif
  !enddo

  open(unit = 24, file = 'xlsm/quantile1.txt')
  read(unit=24, fmt = *) n
  allocate(qs(n), res(n))
  do i = 1, n
     q = quantile(arr, qs(i))
     print *, res(i), q
  enddo
  print *, "TODO"
  
     
end subroutine test_quantile

end program maff90
