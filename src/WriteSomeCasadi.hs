{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import qualified WriteC as C
import qualified WriteHs as HS
import CasadiTree
import Types

main :: IO ()
main = do
  let cOut = init $ unlines $
             "#include <casadi.hpp>\n#include \"../marshall.hpp\"\n" :
             concatMap C.writeClass classes ++
             map C.writeFunction tools
      hsOut = HS.writeModule "Test" classes tools

  writeFile "../cbits/gen/test.cpp" cOut
  writeFile "../dist-src/Gen/Test.hs" hsOut

classes :: [Types.Class]
classes = [sxfun, sxmat, mx]
