FC = nvfortran
FFLAGS = -fast -mp=gpu -gpu=cc61,lineinfo,managed -Minfo=all -cuda -acc

all: test1 test2 test3

test1: test1.f90
	$(FC) $(FFLAGS) -o $@ $<

test2: test2.f90
	$(FC) $(FFLAGS) -o $@ $<

test3: test3.f90
	$(FC) $(FFLAGS) -o $@ $<

