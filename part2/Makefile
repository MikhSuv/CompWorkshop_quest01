FC = gfortran
# FC = flang
OPT = -O3 #-funroll-loops -march=native -fopenmp 

main : main.o precision_mod.o tridiagonal_matrix.o
	$(FC) -o $@ $^ $(OPT)
main.o : app/main.f90 precision_mod.o tridiagonal_matrix.o
	$(FC) -c $< $(OPT)
precision_mod.o : src/precision_mod.f90
	$(FC) -c $^ $(OPT)
tridiagonal_matrix.o : src/tridiagonal_matrix.f90 precision_mod.o
	$(FC) -c $< $(OPT)

clean: 
	rm -rf main *.o *.mod

.PHONY: clean
