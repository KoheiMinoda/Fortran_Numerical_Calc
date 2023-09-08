module procedures

contains
subroutine d2b (argd)
implicit none
real(kind = 8), intent(in) :: argd
real(kind = 8) :: d
integer(kind = 8) :: u, one, b
integer :: i
equivalence (d, u)

one = 1
d = argd

write(*, fmt = "(1x, a, f10.6a)") "d2b", d, ")"
write(*, fmt = "(1x, z16)") d

do i = 63, 0, -1
b = and(rshift(u, i), one)
if(i == 63) then
if(b == 0) then
write(*, fmt = "(1x, a)", advance = "no") "+"
else
write(*, fmt = "(1x, a)", advance = "no") "-"
endif
else
write(*, fmt = "(i1)", advance = "no") b
endif
if(i == 52) write(*, fmt = "(a)", advance = "no") "."
enddo
write(*,*)""

end subroutine d2b
end module procedures


program main

use procedures
implicit none
real(kind = 8) :: Su

Su = 0.1d0 + 0.2d0 + 0.3d0

if(Su == 0.6d0) then
write(*, fmt = "(a)") "Su = 0.6"
else
write(*, fmt = "(a)") "Su != 0.6"
endif

write(*, fmt = "(a)") "double to binary for Su"
call d2b(Su)
write(*, fmt = "(a)") "double to binary for 0.6"
call d2b(0.6d0)

stop

end program main
