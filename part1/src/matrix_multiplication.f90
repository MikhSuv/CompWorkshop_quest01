module matrix_multiplication
  use precision_mod
  implicit none
  private

  integer, parameter :: NB = 128 ! размер блока 
  public :: matrix_mult
contains

  subroutine block_multiply(a, b, c, n, ii, jj, kk)
    ! Вычилсяет произведение блоков
    real(dp), intent(in)  :: a(n,n), b(n,n)
    real(dp), intent(inout) :: c(n,n)
    integer, intent(in)  :: n, ii, jj, kk
    integer :: i, j, k, i_end, j_end, k_end

    i_end = min(ii+NB-1, n)
    j_end = min(jj+NB-1, n)
    k_end = min(kk+NB-1, n)

    do i = ii, i_end
      do j = jj, j_end
      !$OMP SIMD
        do k = kk, k_end
          c(i,j) = c(i,j) + a(i,k) * b(k,j)
        end do
      end do
    end do
  end subroutine block_multiply

  function matrix_mult(a, b, n) result(c)
    ! Вычисляет произведение квадратных матриц A и B
    real(dp), intent(in) :: a(n,n), b(n,n)
    integer, intent(in) :: n
    real(dp) :: c(n,n)

    integer ::  i, j, k

    c = 0.0_dp

    ! Параллельное умножение по блокам
    !$OMP PARALLEL DO COLLAPSE(2) DEFAULT(NONE) &
    !$OMP SHARED(a,b,c,n) PRIVATE(i,j,k)
    do i = 1, n, NB
      do j = 1, n, NB
        do k = 1, n, NB
          call block_multiply(a, b, c, n, i, j, k)
        end do
      end do
    end do
    !$OMP END PARALLEL DO 

  end function matrix_mult

end module matrix_multiplication
