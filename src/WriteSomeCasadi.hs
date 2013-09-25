{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import qualified WriteC as C
--import qualified WriteHs as HS
import CasadiTree

main :: IO ()
main = do
  let cOut = init $ unlines $
             "#include <casadi.hpp>\n#include \"../marshall.hpp\"\n" : C.writeClass fx ++ C.writeClass sxfun ++ map C.writeFunction tools
--      hsOut = HS.writeModule "Test" [sxfun, sxmat, mx] tools

  writeFile "../cbits/gen/test.cpp" cOut
--  writeFile "../dist-src/Gen/Test.hs" hsOut
