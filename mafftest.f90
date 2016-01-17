! test the maff routines
program maff90

use maff
implicit none

print *, "test of maff"


!print *, "days 1900: ..."
!print *, "1900-01-01: ", days1900(1900,1,1)
!print *, "2014-12-27: ", days1900(2014,12,27)

call test_dates
call test_sortd
end program maff90

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
