program main
  use precision_mod
  use matrix_multiplication
  use matrixio
  implicit none

  real(dp), allocatable :: a(:,:), b(:,:), c(:,:) ! квадратная матрица n*n
  integer :: n

  call read_matrix("data1.dat", a, n)
  call read_matrix("data2.dat", b, n)
  c = matrix_mult(a, b, n)
  call write_matrix("result.dat", c, n)

  deallocate(a, b, c)

end program main
