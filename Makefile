all : Casadi/Wrappers/Tools.o allcpp
allcpp : cbits/autogen/all.o cbits/hs_tools.o cbits/callback.o

CASADI_INCLUDES = -I/usr/include/casadi -I/usr/share/casadi
#CASADI_INCLUDES = -I/home/ghorn/casadi -I/home/ghorn/casadi/build/swig

cbits/hs_tools.o : cbits/hs_tools.cpp
	clang++ -Wall -Werror $(CASADI_INCLUDES) -c cbits/hs_tools.cpp -o cbits/hs_tools.o
	@echo "hs_tools.o: no clang errors"

cbits/callback.o : cbits/callback.cpp
	clang++ -Wall -Werror  -Wno-delete-non-virtual-dtor $(CASADI_INCLUDES) -c cbits/callback.cpp -o cbits/callback.o
	@echo "callback.o: no clang errors"

cbits/autogen/all.o : cbits/autogen/all.cpp cbits/marshal.hpp
	clang++ -Wall -Werror -Wno-delete-non-virtual-dtor $(CASADI_INCLUDES) -c cbits/autogen/all.cpp -o cbits/autogen/all.o
	@echo "all.o: no clang errors"

gen : Casadi/Wrappers/Tools.hs cbits/autogen/all.cpp

Casadi/Wrappers/Tools.o : gen Casadi/*.hs
	ghc --make Casadi/*.hs
	ghc --make Casadi/Wrappers/*.hs
	ghc --make Casadi/Wrappers/Classes/*.hs

cbits/autogen/all.cpp Casadi/Wrappers/Tools.hs : writeThemBindings
	./writeThemBindings

writeThemBindings :  WriteBindings.hs WriteBindings/*.hs WriteBindings/Buildbot/*.hs
	ghc --make -O2 WriteBindings -o writeThemBindings

clean_gen :
	rm -f cbits/autogen/*
	rm -f Casadi/Wrappers/*

clean :
	rm -f WriteBindings/Buildbot/*.hi WriteBindings/Buildbot/*.o
	rm -f WriteBindings/*.hi WriteBindings/*.o
	rm -f writeThemBindings WriteBindings.hi WriteBindings.o
	rm -f cbits/*.o cbits/autogen/*
	rm -f Casadi/*.hi Casadi/*.o Casadi/Wrappers/Classes/*
	rm -f Casadi/Wrappers/*.hs Casadi/Wrappers/*.hi Casadi/Wrappers/*.o Casadi/Wrappers/*.txt
