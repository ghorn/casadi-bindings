casadi-bindings
===============

haskell bindings to the CasADi algorithmic differentiation and optimal control library

This project is mainly a bindings generation tool, which I use to generate
cabal packages for release on hackage. You should really try to use the hackage
releases instead of this repository by following the instructions here:
http://hackage.haskell.org/package/casadi-bindings

.

That said, the wrappers are generated with:

- `make gen`

which uses the Makefile in this directory. I think hand-edit the .cabal file and release on hackage.

.

I have managed to get this working on OSX, but it requires hacking the .cabal
file and changing dependencies. This is totally unsupported and unrecommended for now.
