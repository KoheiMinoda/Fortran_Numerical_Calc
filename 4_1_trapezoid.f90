module procedures
contains
real(kind=8) function f (x)
implicit none
real(kind=8), intent(in) :: x
f = log(x)/x
end function f
end module procedures

program main
use procedures
implicit none
real(kind=8) :: a, b, h, s, T, T_new, T_ans, EPS
integer :: N, i
a = 2.0d0
b = 5.0d0
EPS = 1.0d-6
N = 1
h = b - a
T = h * (f(a) + f(b) / 2.0d0)
do while (.true.)
N = 2 * N
h = h / 2.0d0
s = 0.0d0
do i = 1, N-1, 2
s = s + f(a + dble(i) * h)
end do
T_new = T / 2.0 + h * s
if(dabs(T_new - T) < EPS * dabs(T_new)) exit
write(*, fmt='(a, i0, a, e14.7, a, e14.7, a, e14.7)') &
"N: ", N, ", T_new:", T_new, ", abs(T_new - T):", dabs(T_new - T), &
", EPS*abs(T_new):", EPS * dabs(T_new)
T = T_new
end do

T_ans = 1.0d0 / 2.0d0 * (log(b) * log(b) - log(a) * log(a))

write(*, fmt='(a, e14.7, a, e14.7, a, e14.7)') &
"T_new:", T_new, ", T_ans:", T_ans, ", Diff:", T_new - T_ans
stop
end program main
