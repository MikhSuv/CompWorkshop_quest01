FC = gfortran
# FC = flang
OPT = -O3 -funroll-loops -march=native -fopenmp 

main : main.o precision_mod.o matrixio.o matrix_multiplication.o
	$(FC) -o $@ $^ $(OPT)
main.o : app/main.f90 precision_mod.o matrixio.o matrix_multiplication.o
	$(FC) -c $< $(OPT)
precision_mod.o : src/precision_mod.f90
	$(FC) -c $^ $(OPT)
matrixio.o : src/matrixio.f90 precision_mod.o
	$(FC) -c $< $(OPT)
matrix_multiplication.o : src/matrix_multiplication.f90 precision_mod.o
	$(FC) -c $< $(OPT)

clean: 
	rm -rf main *.o *.mod

.PHONY: clean
