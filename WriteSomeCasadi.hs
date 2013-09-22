{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Types
import WriteC


vectorSXMat :: Param
vectorSXMat = Param "std::vector<CasADi::SXMatrix>&"

int :: Param
int = Param "int"

stdStrRef :: Param
stdStrRef = Param "std::string&"


fx :: Class
fx = Class (Name "CasADi::FX") methods
  where
    methods =
      [ Method (Name "getNumScalarInputs") (SimpleType "int") [] (Const False) (Static False)
      ]

tools :: [Function]
tools =
  [ Function (Name "CasADi::ssym") (NewRef "CasADi::SXMatrix") [stdStrRef, int, int]
  , Function (Name "CasADi::msym") (NewRef "CasADi::MX") [stdStrRef, int, int]
  ]

sxfun :: Class
sxfun = Class (Name "CasADi::SXFunction") methods
  where
    methods =
      [ Method (Name "SXFunction") (NewRef "CasADi::SXFunction") [vectorSXMat, vectorSXMat] (Const False) (Static True)
      , Method (Name "jac") (NewRef "CasADi::SXMatrix") [int, int] (Const False) (Static False)
      ]

main :: IO ()
main = do
  let written = init $ unlines $
                "#include <casadi.hpp>\n" : (writeClass fx) ++ (writeClass sxfun) ++ map writeFunction tools

  putStrLn written
  writeFile "test.cpp" written
  putStrLn "wrote test.cpp"
