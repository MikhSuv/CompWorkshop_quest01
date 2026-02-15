module matrixio
  
  use precision_mod
  implicit none
  private

  public :: read_matrix

  contains 

    subroutine read_matrix(filename, matrix, n)
      ! Читает матрицу и ее размер из файла
      character(len=*), intent(in) :: filename ! название файла 
      real(dp), allocatable, intent(out) :: matrix(:,:) ! квадратная матрица n*n
      integer, intent(out) :: n ! порядок матрицы

      integer :: iunit, i, iostatus 
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
      read(line, *) n ! чтение порядка матрицы

      ! Выделение памяти под матрицу
      allocate(matrix(n,n))

      read(iunit, *, iostat = iostatus) matrix
      if (iostatus /= 0) then
        error stop 'Error occured while reading matrix'
      end if

      close(iunit)

    end subroutine read_matrix


end module matrixio
