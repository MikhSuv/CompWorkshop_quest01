program main
  use precision_mod
  use matrix_multiplication
  use matrixio
  implicit none

  real(dp), allocatable, dimension(:,:) :: a, b, c, d ! квадратная матрица n*n
  integer :: n

  call read_matrix("data1.dat", a, n)
  call read_matrix("data2.dat", b, n)
  c = matrix_mult(a, b, n)
  call write_matrix("result.dat", c, n)
  print *, "Записал результат в result.dat"

  deallocate(a, b, c)

  n = 10000

  allocate(A(n,n))
  allocate(B(n,n))

  call random_number(A)
  print *, "Сгенерировал матрицу A"
  call random_number(B)
  print *, "Сгенерировал матрицу B"
  print *, "Вычисление произведения функцией matmul"
  C = matmul(A, B) ! Эталонное значение
  print *, "Вычисление законченно"

  print *, "Вычисление произведения функцией matrix_mult"
  D = matrix_mult(a, b, n)
  print *, "Вычисление законченно"
  D = abs(D-C) ! Сравнение с эталоном
  print *,"Mаксимальная погрешность:", maxval(D) ! Выводит максимальную погрешность
  deallocate(a,b,c,d)

end program main
