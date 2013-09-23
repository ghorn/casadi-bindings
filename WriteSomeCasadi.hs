{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import WriteC
import CasadiTree

main :: IO ()
main = do
  let written = init $ unlines $
                "#include <casadi.hpp>\n" : (writeClass fx) ++ (writeClass sxfun) ++ map writeFunction tools

  writeFile "gen/test.cpp" written
