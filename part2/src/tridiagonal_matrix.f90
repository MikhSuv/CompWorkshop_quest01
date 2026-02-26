module tridiagonal_matrix

  use precision_mod
  implicit none
  private
  public :: tridiag_matrix, read_tdmatrix, print_matrix

  type :: tridiag_matrix
    private
    integer :: n = 0
    real(dp), allocatable :: main_diag(:) ! Главная диагональ
    real(dp), allocatable :: lower_diag(:) ! Нижняя диагональ
    real(dp), allocatable :: upper_diag(:) ! Верхняя диагональ
  contains
    procedure :: read_tdmatrix 
    procedure :: print_matrix
    ! procedure :: tdmatmul ! TODO: OOP
  end type tridiag_matrix


  contains
    subroutine read_tdmatrix(matrix, filename)
      character(len=*), intent(in) :: filename
      class(tridiag_matrix), intent(out) :: matrix 

      integer :: iunit, iostatus, i
      character(len=256) :: line ! для чтения первой строки

      open(newunit=iunit, file=filename, status='old', &
      action = 'read', iostat=iostatus)
      if (iostatus /= 0) then
        error stop 'Error occured while opening file'
      end if

      read(iunit, '(a)', iostat=iostatus) line

      if (iostatus /= 0) then
        error stop 'Error occured while reading line'
      end if
      line = adjustl(line) !Удаление пробелов слева и добавление их в конец
      if (line(1:1) == "#") line = line(2:) ! пропуск '#'
      read(line, *) matrix%n ! чтение порядка матрицы

      allocate(matrix%main_diag(matrix%n))
      allocate(matrix%lower_diag(matrix%n-1))
      allocate(matrix%upper_diag(matrix%n-1))

      read(iunit, *, iostat=iostatus) matrix%main_diag(1), matrix%upper_diag(1)
      if (iostatus /= 0) then
        error stop 'Error occured while reading matrix'
      end if

      do i = 2, matrix%n-1
        read(iunit, *, iostat = iostatus) matrix%lower_diag(i-1), &
          matrix%main_diag(i), matrix%upper_diag(i)
        if (iostatus /= 0) then
          error stop 'Error occured while reading matrix'
        end if
      end do
      
      read(iunit, *, iostat = iostatus) matrix%lower_diag(matrix%n-1), matrix%main_diag(matrix%n)
      if (iostatus /= 0) then
        error stop 'Error occured while reading matrix'
      end if
      close(iunit)

    end subroutine read_tdmatrix

    subroutine print_matrix(matrix)
      class(tridiag_matrix), intent(in) :: matrix 
      
      print *,'Главная', matrix%main_diag
      print *, 'Нижняя', matrix%lower_diag
      print *, 'Верхняя', matrix%upper_diag
    end subroutine print_matrix

end module tridiagonal_matrix 

