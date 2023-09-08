module procedures

contains
real(kind = 8) function f(x)
implicit none
real(kind = 8), intent(in) :: x
f = log(x)
end function f

end module procedures


program main

use procedures
implicit none
integer :: i
real(kind = 8) :: a, b, c, fc, EPS
a = 0.5d0
b = 2.0d0
EPS = 1.0d-8
i = 1

do while(.true.)
c = (a+b) / 2.0d0
write(*, fmt = "(i0, 3(a, e12.6))") i, "// a:", a, "/ b:", b, "/ c:", c
write(*, fmt = "(2x, a, e14.6)") "diff:", dabs(a-b) / 2.0d0
if (dabs(a-b) / 2.0d0 < EPS) exit
fc = f(c)
write(*, fmt = "(2x, a, e14.6)") "fc:", fc
if(fc > 0.0d0) b = c 
if(fc < 0.0d0) a = c 
if(fc == 0.0d0) exit
i = i + 1
end do
write(*, fmt = "(a, e14.6)") "root of log_e(x) is:", c

stop
end program
