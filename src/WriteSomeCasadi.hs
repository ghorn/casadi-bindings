{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import qualified WriteC as C
import qualified WriteHs as HS
import CasadiTree
import Types

main :: IO ()
main = do
  let cOut = init $ unlines $
             [ "#include <build/swig/swiginclude.hpp>"
             , "#include \"../marshal.hpp\""
             ] ++
             concatMap C.writeClass classes ++
             map (C.writeFunction . addNamespace) tools ++ map C.writeDeletes [CInt,CDouble,StdString,CBool]
      hsOut = HS.writeModule "Test" classes tools

  length  cOut `seq` writeFile "../cbits/gen/test.cpp" cOut
  length hsOut `seq` writeFile "../dist-src/Gen/Test.hs" hsOut

addNamespace :: Function -> Function
addNamespace (Function (Name name) x y) = Function (Name ("CasADi::"++name)) x y
