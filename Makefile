all : cbits/gen/test.o cbits/marshal.o cbits/hs_tools.o dist-src/Gen/Test.o
cpp : cbits/gen/test.o cbits/marshal.o cbits/hs_tools.o

CASADI_PATH = /home/ghorn/casadi

CASADI_INCLUDES = -I$(CASADI_PATH)

emit-c : cbits/gen/test.cpp
	clear
	@pygmentize -f terminal -g cbits/gen/test.cpp

emit-hs : dist-src/Gen/Test.hs
	clear
	@pygmentize -f terminal -g dist-src/Gen/Test.hs

cbits/gen/test.o : cbits/gen/test.cpp cbits/marshal.hpp
	clang++ -Wall -Werror -Wno-delete-non-virtual-dtor $(CASADI_INCLUDES) -c cbits/gen/test.cpp -o cbits/gen/test.o
	@echo "test.o: no clang errors"

cbits/marshal.o : cbits/marshal.cpp cbits/marshal.hpp
	clang++ -Wall -Werror $(CASADI_INCLUDES) -c cbits/marshal.cpp -o cbits/marshal.o
	@echo "marshal.o: no clang errors"

cbits/hs_tools.o : cbits/hs_tools.cpp cbits/hs_tools.hpp
	clang++ -Wall -Werror $(CASADI_INCLUDES) -c cbits/hs_tools.cpp -o cbits/hs_tools.o
	@echo "hs_tools.o: no clang errors"

dist-src/Gen/Test.o : dist-src/Gen/Test.hs dist-src/Marshal.hs
	cd dist-src && ghc --make Gen/Test.hs

cbits/gen/test.cpp dist-src/Gen/Test.hs : src/WriteSomeCasadi
	cd src && ./WriteSomeCasadi

src/WriteSomeCasadi : src/*.hs
	cd src && ghc --make -O2 WriteSomeCasadi

clean :
	rm -f src/*.hi src/*.o src/WriteSomeCasadi
	rm -f cbits/*.o cbits/gen/*
	rm -f dist-src/Marshal.hi dist-src/Marshal.o dist-src/Gen/*
