all : test.o

test.o : test.cpp
	clang++ -Wall -c test.cpp -I/home/ghorn/casadi/symbolic
	@echo "clang ran successfully"

test.cpp : *.hs
	runhaskell WriteSomeCasadi.hs

clean :
	rm test.cpp test.o

