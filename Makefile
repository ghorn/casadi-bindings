all : gen/test.o

emit : gen/test.cpp
	clear
	@pygmentize -f terminal -g gen/test.cpp

gen/test.o : gen/test.cpp
	clang++ -Wall -Werror -I/home/ghorn/casadi/symbolic -c gen/test.cpp -o gen/test.o
	@echo "no clang errors"

gen/test.cpp : *.hs
	runhaskell WriteSomeCasadi.hs

clean :
	rm -f gen/test.cpp gen/test.o
