program main

implicit none
real(kind = 8) :: a, b, c, x
a = 1.0d-15; c = 2.0d-15; b = 3.0d15

!donot use a countermeasure
x = (-b - sqrt(b*b - 4.0d0*a*c)) / (2.0d0*a)
write(*, fmt = "(a, e14.7)") "x =", x
write(*, fmt = "(2x, a, e14.7/)") "a x*X + b x + c =", a*x*x + b*x + c

!use a countermeasure
x = (-2.0d0 * a) / (b + sqrt(b*b - 4.0d0*a*c))
write(*, fmt = "(a, e14.7)") "Improved x =", x
write(*, fmt = "(2x, a, e14.7/)") "a x*X + b x + c =", a*x*x + b*x + c
stop

end program main
