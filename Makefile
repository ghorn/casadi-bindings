all : cbits/gen/test.o dist-src/Gen/Test.o

emit-c : cbits/gen/test.cpp
	clear
	@pygmentize -f terminal -g cbits/gen/test.cpp

emit-hs : dist-src/Gen/Test.hs
	clear
	@pygmentize -f terminal -g dist-src/Gen/Test.hs

cbits/gen/test.o : cbits/gen/test.cpp
	clang++ -Wall -Werror -Wno-delete-non-virtual-dtor -I/home/ghorn/casadi/symbolic -c cbits/gen/test.cpp -o cbits/gen/test.o
	@echo "no clang errors"

dist-src/Gen/Test.o : dist-src/Gen/Test.hs dist-src/Marshall.hs
	cd dist-src && ghc --make Gen/Test.hs

cbits/gen/test.cpp dist-src/Gen/Test.hs : src/*.hs
	cd src && runhaskell WriteSomeCasadi.hs

clean :
	rm -f cbits/gen/* dist-src/Gen/*
