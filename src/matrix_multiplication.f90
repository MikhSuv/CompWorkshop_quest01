module matrix_multiplication
  use precision_mod
  implicit none
  private

  integer, parameter :: NB = 128 ! размер блока 
  public :: matrix_mult, matmul_opt
contains

  function matrix_mult(first_matrix, second_matrix, n) result(prod) ! TODO: исключить n из аргументов
    ! Вычисляет произведение квадратных матриц 
    real(dp), dimension(n, n), intent(in) :: first_matrix, second_matrix 
    integer, intent(in) :: n
    real(dp) :: prod(n,n)

    integer :: i, j, k

    do i = 1, n
      do j = 1, n 
        prod(i, j) = 0.0_dp
        do k = 1, n
          prod(i, j) = prod(i, j) + second_matrix(k, j)*first_matrix(i, k)
        end do
      end do
    end do
    
  end function matrix_mult

  subroutine block_multiply(a, b, c, n, ii, jj, kk)
    real(dp), intent(in)  :: a(n,n), b(n,n)
    real(dp), intent(inout) :: c(n,n)
    integer, intent(in)  :: n, ii, jj, kk
    integer :: i, j, k, i_end, j_end, k_end

    i_end = min(ii+NB-1, n)
    j_end = min(jj+NB-1, n)
    k_end = min(kk+NB-1, n)

    do i = ii, i_end
      do j = jj, j_end
        ! Векторизованный внутренний цикл
        !$OMP SIMD
        do k = kk, k_end
          c(i,j) = c(i,j) + a(i,k) * b(k,j)
        end do
      end do
    end do
  end subroutine block_multiply

  function matmul_opt(a, b, n) result(c)
    real(dp), intent(in), dimension(n,n) :: a, b
    integer, intent(in) :: n
    real(dp) :: c(n,n)

    integer :: i, j, k, ii, jj, kk

    ! Параллельное обнуление матрицы
    !$OMP PARALLEL DO COLLAPSE(2) SCHEDULE(STATIC)
    do j = 1, n
      do i = 1, n
        c(i,j) = 0.0_dp
      end do
    end do
    !$OMP END PARALLEL DO

    !Умножение по блокам 
    ! Основное умножение по блокам
    !$OMP PARALLEL DO COLLAPSE(2) PRIVATE(ii, jj, kk, i, j, k) SCHEDULE(STATIC)
    do ii = 1, n, NB
      do jj = 1, n, NB
        do kk = 1, n, NB
          call block_multiply(a, b, c, n, ii, jj, kk)
        end do
      end do
    end do
    !$OMP END PARALLEL DO

  end function matmul_opt

end module matrix_multiplication
