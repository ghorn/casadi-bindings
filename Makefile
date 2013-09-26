all : cbits/gen/test.o cbits/marshal.o cbits/hs_tools.o CasadiBindings/Gen/Test.o
cpp : cbits/gen/test.o cbits/marshal.o cbits/hs_tools.o

CASADI_PATH = /home/ghorn/casadi

CASADI_INCLUDES = -I$(CASADI_PATH)

emit-c : cbits/gen/test.cpp
	clear
	@pygmentize -f terminal -g cbits/gen/test.cpp

emit-hs : CasadiBindings/Gen/Test.hs
	clear
	@pygmentize -f terminal -g CasadiBindings/Gen/Test.hs

cbits/gen/test.o : cbits/gen/test.cpp cbits/marshal.hpp
	clang++ -Wall -Werror -Wno-delete-non-virtual-dtor $(CASADI_INCLUDES) -c cbits/gen/test.cpp -o cbits/gen/test.o
	@echo "test.o: no clang errors"

cbits/marshal.o : cbits/marshal.cpp cbits/marshal.hpp
	clang++ -Wall -Werror $(CASADI_INCLUDES) -c cbits/marshal.cpp -o cbits/marshal.o
	@echo "marshal.o: no clang errors"

cbits/hs_tools.o : cbits/hs_tools.cpp cbits/hs_tools.hpp
	clang++ -Wall -Werror $(CASADI_INCLUDES) -c cbits/hs_tools.cpp -o cbits/hs_tools.o
	@echo "hs_tools.o: no clang errors"

CasadiBindings/Gen/Test.o : CasadiBindings/Gen/Test.hs CasadiBindings/Marshal.hs
	ghc --make CasadiBindings/Gen/Test.hs

cbits/gen/test.cpp CasadiBindings/Gen/Test.hs : WriteBindings
	./WriteBindings

WriteBindings : WriteCasadiBindings/*.hs WriteBindings.hs
	ghc --make -O2 WriteBindings

clean :
	rm -f WriteCasadiBindings/*.hi WriteCasadiBindings/*.o
	rm -f WriteBindings WriteBindings.hi WriteBindings.o
	rm -f cbits/*.o cbits/gen/*
	rm -f CasadiBindings/*.hi CasadiBindings/*.o CasadiBindings/Gen/*
