program check
  use precision_mod
  use matrix_multiplication
  use matrixio

implicit none

real(dp), dimension(:, :), allocatable :: A, B, C,D
integer, parameter :: n = 2048

allocate(A(n,n))
allocate(B(n,n))

call random_number(A)
call random_number(B)
C = matmul(A, B)
D = matrix_mult(a, b,n)
D = abs(D -C)
print *, maxval(D)
deallocate(a,b,c,d)

end program check
