{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Casadi/Overloading.hs"]
