{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import WriteC
--import WriteHs
import CasadiTree

main :: IO ()
main = do
  let written = init $ unlines $
                "#include <casadi.hpp>\n" : (writeClass fx) ++ (writeClass sxfun) ++ map writeFunction tools

  putStrLn written
  writeFile "test.cpp" written
  putStrLn "wrote test.cpp"
