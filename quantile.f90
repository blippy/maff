!> calculates quantiles according to Excel interpolation formula
! https://en.wikipedia.org/wiki/Quantile
! assumes array is sorted

function quantile(arr, q)
  double precision :: arr(:), q

  double precision :: hd
  integer :: hi

  hd = (dble(size(arr))-1.0d0) * q + 1.0d0
  hi = floor(hd)
  if(q.eq.1.0d0) then
     quantile = arr(size(arr))
     return
  endif
  quantile = arr(hi) + (hd - dble(hi)) * (arr(hi+1) - arr(hi))
  !integer:: idx0
  !double precision :: didx0

  !didx0 = (dble(size(arr))-1.0d0) * q + 1.0d0
  !idx0 = ceiling(didx0)
  !if(idx0.eq.didx0) then
  !   quantile = arr(idx0)
  !   return
  !endif

  !v1 = arr(idx0)
  !v2 = arr(idx0+1)
  !quantile = v1 * ? + v2 *
  
end function quantile
