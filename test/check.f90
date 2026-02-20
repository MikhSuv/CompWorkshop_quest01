program check
  use precision_mod
  use matrix_multiplication
  use matrixio

implicit none

real(dp), dimension(:, :), allocatable :: A, B, C,D
integer, parameter :: n = 4096

allocate(A(n,n))
allocate(B(n,n))

call random_number(A)
print *, "Сгенерил матрицу А"
call random_number(B)
print *, "Сгенерил матрицу B"
C = matmul(A, B)
D = matmul_opt(a, b,n)
print *, "matrix_mult ВСЁ"
D = abs(D -C)
print *, maxval(D)
deallocate(a,b,c,d)

end program check
