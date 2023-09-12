module procedures

contains
subroutine MyLUsolve (n, A, b, u)

implicit none
integer, intent(in) :: n
real(kind = 8), dimension(:,:), intent(inout) :: A
real(kind = 8), dimension(:), intent(in) :: b
real(kind = 8), dimension(:), intent(inout) :: u
real(kind = 8), dimension(n) :: c
integer :: i, j, k
real(kind = 8) :: dtemp

do k = 1, n-1
dtemp = 1.0 / A(k, k)
do i = k + 1, n
A(i,k) = A(i, k)*dtemp
end do
do j = k+1, n
dtemp = A(j,k)
do i = k+1, n
A(j,i) = A(j,i) - A(k,i)*dtemp
end do
end do
end do

!zenshin
do k = 1, n
c(k) = b(k)
do j=1, k-1
c(k) = c(k) - A(k,j)*c(j)
end do
end do

!koutai
u(n) = c(n)/A(n,n)
do k = n-1, 1, -1
u(k) = c(k)
do j = k+1, n
u(k) = u(k) - A(k,j)*u(j)
end do
u(k) = u(k) / A(k,k)
end do

end subroutine MyLUsolve

end module procedures
