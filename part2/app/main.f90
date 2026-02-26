program main
  use precision_mod
  use tridiagonal_matrix

  implicit none
  
  type(tridiag_matrix) :: A

  call A%read_tdmatrix('data1.txt')
  call A%print_matrix()
end program main
