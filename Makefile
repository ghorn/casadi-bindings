all : cbits/gen/test.o cbits/marshall.o dist-src/Gen/Test.o
cpp : cbits/gen/test.o cbits/marshall.o

emit-c : cbits/gen/test.cpp
	clear
	@pygmentize -f terminal -g cbits/gen/test.cpp

emit-hs : dist-src/Gen/Test.hs
	clear
	@pygmentize -f terminal -g dist-src/Gen/Test.hs

cbits/gen/test.o : cbits/gen/test.cpp cbits/marshall.hpp
	clang++ -Wall -Werror -Wno-delete-non-virtual-dtor -I/home/ghorn/casadi/symbolic -c cbits/gen/test.cpp -o cbits/gen/test.o
	@echo "test.o: no clang errors"

cbits/marshall.o : cbits/marshall.cpp cbits/marshall.hpp
	clang++ -Wall -Werror -I/home/ghorn/casadi/symbolic -c cbits/marshall.cpp -o cbits/marshall.o
	@echo "marshall.o: no clang errors"

dist-src/Gen/Test.o : dist-src/Gen/Test.hs dist-src/Marshall.hs
	cd dist-src && ghc --make Gen/Test.hs

cbits/gen/test.cpp dist-src/Gen/Test.hs : src/*.hs
	cd src && runhaskell WriteSomeCasadi.hs

clean :
	rm -f cbits/marshall.o cbits/gen/* dist-src/Marshall.hi dist-src/Marshall.o dist-src/Gen/*
