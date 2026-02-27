module tridiagonal_matrix

  use precision_mod
  implicit none
  private
  public :: tdmatmul

  type, public :: tridiag_matrix
    private
    integer :: n = 0
    real(dp), allocatable :: main_diag(:) ! Главная диагональ
    real(dp), allocatable :: lower_diag(:) ! Нижняя диагональ
    real(dp), allocatable :: upper_diag(:) ! Верхняя диагональ
  contains
    procedure :: read_tdmatrix 
    procedure :: print_matrix
    procedure :: generate
    ! procedure :: tdmatmul ! TODO: OOP
  end type tridiag_matrix

  type, public :: pentadiag_matrix
    private
    integer :: n = 0
    real(dp), allocatable :: diags(:, :) !матрица 5*n, 5 строк -2, -1, 0, 1, 2 диагонали
  contains
     procedure :: write_pdmatrix

  end type pentadiag_matrix


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

      allocate(matrix%main_diag(matrix%n)) ! Выделение памяти под диагонали
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

    subroutine generate(matrix, n)
      ! Заполняет матрицу случайными числами из [0,1]
      class(tridiag_matrix), intent(out) :: matrix
      integer, intent(in) :: n
      if (n <= 0) error stop 'N have to be positive'
      matrix%n = n
      allocate(matrix%main_diag(n))
      allocate(matrix%lower_diag(n-1))
      allocate(matrix%upper_diag(n-1))
      call random_number(matrix%main_diag)
      call random_number(matrix%upper_diag)
      call random_number(matrix%lower_diag)
    end subroutine generate

    function tdmatmul(A, B) result(C)
      type(tridiag_matrix), intent(in) :: A,B
      type(pentadiag_matrix) :: C

      integer :: i, n
      n = A%n
      C%n = n
      allocate(C%diags(5, n), source=0.0_dp)

    ! Главная диагональ
      do i = 1, n
        C%diags(3, i) = A%main_diag(i) * B%main_diag(i)
        if (i > 1) then
            C%diags(3, i) = C%diags(3, i) + A%lower_diag(i-1) * B%upper_diag(i-1)
        end if
        if (i < n) then
            C%diags(3, i) = C%diags(3, i) + A%upper_diag(i) * B%lower_diag(i)
        end if
      end do
      ! Первая наддиагональ
      do i = 1, n-1
        C%diags(4, i) = A%main_diag(i) * B%upper_diag(i) + A%upper_diag(i) * B%main_diag(i+1)
      end do
      ! Вторая наддиагональ
      do i = 1, n-2
        C%diags(5, i) = A%upper_diag(i) * B%upper_diag(i+1)
      end do
      ! Первая поддиагональ
      do i = 2, n
        C%diags(2, i) = A%lower_diag(i-1) * B%main_diag(i-1) + A%main_diag(i) * B%lower_diag(i-1)
      end do
      ! Вторая поддиагональ
      do i = 3, n
        C%diags(1, i) = A%lower_diag(i-1) * B%lower_diag(i-2)
      end do
    end function tdmatmul

    subroutine write_pdmatrix(matrix, filename)
      class(pentadiag_matrix), intent(in) :: matrix
      character(len=*), intent(in) :: filename
      integer :: i, ounit, iostatus

      open(newunit=ounit, file=filename, action='write', iostat=iostatus)
      if (iostatus /= 0) then
        error stop 'Error occured while opening file'
      end if

      write(ounit, '("# ", i0)') matrix%n
do i = 1, matrix%n
      if (i == 1) then
        write(ounit, '(3(f12.6,1x))') matrix%diags(3:5, i)
      else if (i == 2) then
        write(ounit, '(4(f12.6,1x))') matrix%diags(2:5, i)
      else if (i == matrix%n - 1) then
        write(ounit, '(4(f12.6,1x))') matrix%diags(1:4, i)
      else if (i == matrix%n) then
        write(ounit, '(3(f12.6,1x))') matrix%diags(1:3, i)
      else
        write(ounit, '(5(f12.6,1x))') matrix%diags(1:5, i)
      end if
    end do
      close(ounit)
    end subroutine write_pdmatrix

end module tridiagonal_matrix 

