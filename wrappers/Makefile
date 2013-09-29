all : Casadi/Wrappers/Autogen/All.o allcpp
allcpp : cbits/autogen/all.o cbits/hs_tools.o

CASADI_PATH = /home/ghorn/casadi

CASADI_INCLUDES = -I$(CASADI_PATH)

cbits/hs_tools.o : cbits/hs_tools.cpp
	clang++ -Wall -Werror $(CASADI_INCLUDES) -c cbits/hs_tools.cpp -o cbits/hs_tools.o
	@echo "hs_tools.o: no clang errors"

cbits/autogen/all.o : cbits/autogen/all.cpp cbits/marshal.hpp
	clang++ -Wall -Werror -Wno-delete-non-virtual-dtor $(CASADI_INCLUDES) -c cbits/autogen/all.cpp -o cbits/autogen/all.o
	@echo "all.o: no clang errors"

gen_hs : Casadi/Wrappers/Autogen/Data.hs

Casadi/Wrappers/Autogen/*.o : gen_hs Casadi/Wrappers/*.hs
	ghc --make Casadi/Wrappers/Autogen/*.hs

cbits/autogen/all.cpp Casadi/Wrappers/Autogen/Data.hs : WriteBindings
	./WriteBindings

WriteBindings : WriteCasadiBindings/*.hs WriteBindings.hs
	ghc --make -O2 WriteBindings

clean_gen :
	rm -f cbits/autogen/*
	rm -f Casadi/Wrappers/Autogen/*

clean :
	rm -f WriteCasadiBindings/*.hi WriteCasadiBindings/*.o
	rm -f WriteBindings WriteBindings.hi WriteBindings.o
	rm -f cbits/*.o cbits/autogen/*
	rm -f Casadi/Wrappers/*.hi Casadi/Wrappers/*.o Casadi/Wrappers/Autogen/*
