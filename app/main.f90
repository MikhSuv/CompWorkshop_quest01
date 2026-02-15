program main
  use precision_mod
  use matrix_multiplication, only: say_hello
  use matrixio
  implicit none

  real(dp), allocatable :: matrix(:,:) ! квадратная матрица n*n
  integer :: n

  call read_matrix("data1.dat", matrix, n)
  write(*,*) n
  call say_hello()
  write(*,*) matrix

end program main
