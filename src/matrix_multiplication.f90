module matrix_multiplication
  use precision_mod
  implicit none
  private

  public :: matrix_mult
contains

  function matrix_mult(first_matrix, second_matrix, n) result(prod) ! TODO: исключить n из аргументов
    ! Вычисляет произведение квадратных матриц 
    real(dp), dimension(n, n), intent(in) :: first_matrix, second_matrix 
    integer, intent(in) :: n
    real(dp) :: prod(n,n)

    integer :: i, j, k

    do i = 1, n
      do j = 1, n 
        prod(i, j) = 0
        do k = 1, n
          prod(i, j) = prod(i, j) + second_matrix(k, j)*first_matrix(i, k)
        end do
      end do
    end do
    
  end function matrix_mult

end module matrix_multiplication
