module procedures
contains
  real(kind=8) function l_j(x, j, N, x_j)
    implicit none
    real(kind=8), intent(in) :: x
    integer, intent(in) :: j, N
    real(kind=8), dimension(:), intent(in) x_j
    integer :: i
    real(kind=8) :: x_u, x_l
    x_u = 1.0d0
    x_l = 1.0d0
    do i = 1, N+1
      if(j /= i) then
	x_u = x_u * (x - x_j(i))
	x_l = x_l * (x_j(j) - x_j(i))
      endif
    enddo
    l_j = x_u / x_l
  end function l_j
end module procedures

program main
  use procedures
  implicit none
  real(kind=8), parameter :: PI = 3.14159265d0
  real(kind=8) :: x, y, x_j(6), y_j(6)
  integer :: N, i, j
  N = 5
  x_j(1) = PI*(1.0d0/4.0d0-1.0d0/3.0d0)
  x_j(2) = PI*(1.0d0/4.0d0+1.0d0/7.0d0)
  x_j(3) = PI*(1.0d0/2.0d0-1.0d0/8.0d0)
  x_j(4) = PI*(1.0d0/4.0d0+1.0d0/6.0d0)
  x_j(5) = PI*(4.0d0/5.0d0)
  x_j(6) = PI*(5.0d0/6.0d0)
  do i=1, N+1
    y_j(i) = cos(x_j(i))
  end do

  do i = 1, 100
    x = -0.5d0 + (PI/100.d0) * dble(i-1)
    y = 0.0d0
    do j = 1, N+1
      y = y + y_j(j) * l_j(x, j, N, x_j)
    end do
    write(*, fmt = '(e14.6, 1x, e14.6)')x,y
  end do
  stop
end program main
