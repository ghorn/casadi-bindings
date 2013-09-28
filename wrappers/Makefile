all : Casadi/Wrappers/Autogen/All.o all-cpp
all-cpp : cbits/autogen/all.o cbits/marshal.o cbits/hs_tools.o

CASADI_PATH = /home/ghorn/casadi

CASADI_INCLUDES = -I$(CASADI_PATH)

emit-c : cbits/autogen/all.cpp
	clear
	@pygmentize -f terminal -g cbits/autogen/all.cpp

emit-hs : Casadi/Wrappers/Autogen/All.hs
	clear
	@pygmentize -f terminal -g Casadi/Wrappers/Autogen/All.hs

cbits/marshal.o : cbits/marshal.cpp cbits/marshal.hpp
	clang++ -Wall -Werror $(CASADI_INCLUDES) -c cbits/marshal.cpp -o cbits/marshal.o
	@echo "marshal.o: no clang errors"

cbits/hs_tools.o : cbits/hs_tools.cpp cbits/hs_tools.hpp
	clang++ -Wall -Werror $(CASADI_INCLUDES) -c cbits/hs_tools.cpp -o cbits/hs_tools.o
	@echo "hs_tools.o: no clang errors"

cbits/autogen/all.o : cbits/autogen/all.cpp cbits/marshal.hpp
	clang++ -Wall -Werror -Wno-delete-non-virtual-dtor $(CASADI_INCLUDES) -c cbits/autogen/all.cpp -o cbits/autogen/all.o
	@echo "all.o: no clang errors"

gen-hs : Casadi/Wrappers/Autogen/All.hs Casadi/Wrappers/Autogen/ForeignToolsImports.hs Casadi/Wrappers/Autogen/ForeignToolsInstances.hs 

Casadi/Wrappers/Autogen/All.o : gen-hs Casadi/Wrappers/*.hs
	ghc --make Casadi/Wrappers/Autogen/All.hs

Casadi/Wrappers/Autogen/ForeignToolsImports.hs Casadi/Wrappers/Autogen/ForeignToolsInstances.hs : WriteCasadiBindings/WriteForeignTools.hs
	cd WriteCasadiBindings && runhaskell WriteForeignTools.hs

cbits/autogen/all.cpp Casadi/Wrappers/Autogen/All.hs : WriteBindings
	./WriteBindings

WriteBindings : WriteCasadiBindings/*.hs WriteBindings.hs
	ghc --make WriteBindings

clean :
	rm -f WriteCasadiBindings/*.hi WriteCasadiBindings/*.o
	rm -f WriteBindings WriteBindings.hi WriteBindings.o
	rm -f cbits/*.o cbits/autogen/*
	rm -f Casadi/Wrappers/*.hi Casadi/Wrappers/*.o Casadi/Wrappers/Autogen/*
