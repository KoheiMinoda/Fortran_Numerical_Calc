module procedures

contains
real(kind = 8) function f(x)
implicit none
real(kind = 8), intent(in) :: x
f = log(x)
end function f
real(kind = 8) function f_prime(x)
implicit none
real(kind = 8), intent(in) :: x
f_prime = 1.0d0 / x
end function f_prime

end module procedures


program main

use procedures
implicit none
integer :: i
real(kind = 8) :: x, x_new, EPS
x = 2.0d0
EPS = 1.0d-8
i = 1

do while(.true.)
x_new = x - f(x)/f_prime(x)
write(*, fmt = "(i0, 2(a, e14.6))") &
i, "// x_new:", x_new, "x_diff:", dabs(x_new-x)/dabs(x_new)
if (dabs(x_new-x) < EPS*dabs(x_new)) exit
x = x_new
i = i + 1
end do
write(*, fmt = "(a, e14.6)") "root of log_e(x) is:", x_new

stop
end program
