casadi-bindings
===============

haskell bindings to the CasADi algorithmic differentiation and optimal control library

# Debian/Ubuntu instructions

- Install libcasadi from a .deb package here: https://github.com/casadi/casadi/releases, I use libcasadi-static. (Get the version corresponding to the current casadi-bindings version, for example casadi-bindings-1.8.0.0 is libcasadi 1.8.0)

- `cabal update; cabal install casadi-bindings`


These wrappers are generated using:

- `cd wrappers`

- `make gen`

I have managed to get this working on OSX, but it requires hacking the .cabal file and changing dependencies. This is totally unsupported and unrecommended for now.
