program main

implicit none
integer :: i
real(kind = 4) :: x

!infoloss
x = 100.0e0
do i = 1, 100000
x = x + 1.0e-6
enddo
write(*, fmt = "(a, f10.6)") "ans (1) x =", x

!not infoloss
x = 0.0e0
do i = 1, 100000
x = x + 1.0e-6
enddo
x = x + 100.0e0
write(*, fmt = "(a, f10.6)") "ans (2) x =", x

stop

end program main
