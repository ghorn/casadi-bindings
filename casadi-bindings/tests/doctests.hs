{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.DocTest

main :: IO ()
main =
  doctest
  [ "-idist/build"
  , "dist/build/cbits/casadi_bindings.o"
  , "src/Casadi/Overloading.hs"
  ]
