all : Casadi/Wrappers/Tools.o allcpp
allcpp : cbits/autogen/all.o cbits/hs_tools.o

CASADI_INCLUDES = -I/usr/include/casadi -I/usr/share/casadi

cbits/hs_tools.o : cbits/hs_tools.cpp
	clang++ -Wall -Werror $(CASADI_INCLUDES) -c cbits/hs_tools.cpp -o cbits/hs_tools.o
	@echo "hs_tools.o: no clang errors"

cbits/autogen/all.o : cbits/autogen/all.cpp cbits/marshal.hpp
	clang++ -Wall -Werror -Wno-delete-non-virtual-dtor $(CASADI_INCLUDES) -c cbits/autogen/all.cpp -o cbits/autogen/all.o
	@echo "all.o: no clang errors"

gen : Casadi/Wrappers/Tools.hs cbits/autogen/all.cpp

Casadi/Wrappers/Tools.o : gen Casadi/*.hs
	ghc --make Casadi/Wrappers/*.hs
	ghc --make Casadi/Wrappers/Classes/*.hs

cbits/autogen/all.cpp Casadi/Wrappers/Tools.hs : WriteBindings
	./WriteBindings

WriteBindings :  WriteBindings.hs WriteCasadiBindings/*.hs WriteCasadiBindings/Buildbot/*.hs
	ghc --make -O2 WriteBindings

clean_gen :
	rm -f cbits/autogen/*
	rm -f Casadi/Wrappers/*

clean :
	rm -f WriteCasadiBindings/Buildbot/*.hi WriteCasadiBindings/Buildbot/*.o
	rm -f WriteCasadiBindings/*.hi WriteCasadiBindings/*.o
	rm -f WriteBindings WriteBindings.hi WriteBindings.o
	rm -f cbits/*.o cbits/autogen/*
	rm -f Casadi/*.hi Casadi/*.o Casadi/Wrappers/Classes/*
	rm -f Casadi/Wrappers/*.hs Casadi/Wrappers/*.hi Casadi/Wrappers/*.o Casadi/Wrappers/*.txt
