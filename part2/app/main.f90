program main
  use precision_mod
  use tridiagonal_matrix

  implicit none
  
  type(tridiag_matrix) :: A, B
  type(pentadiag_matrix) :: C
  ! integer, parameter :: n = 10

  call A%read_tdmatrix('data1.dat')
  call B%read_tdmatrix ('data2.dat')
  ! call A%generate(n)
  ! call B%generate(n)
  call A%print_matrix
  call B%print_matrix()
  C = tdmatmul(A, B)
  call C%write_pdmatrix('result.dat')

end program main
