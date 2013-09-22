{-# OPTIONS_GHC -Wall #-}

module CasadiTree ( fx, sxfun, tools ) where

import Types

vectorSXMat :: Type
vectorSXMat = Ref (Type "std::vector<CasADi::SXMatrix>")

int :: Type
int = Type "int"

stdStrRef :: Type
stdStrRef = Ref (Type "std::string")


fx :: Class
fx = Class (Name "CasADi::FX") methods
  where
    methods =
      [ Method (Name "getNumScalarInputs") (SimpleType int) [] (Const False) (Static False)
      ]

sxfun :: Class
sxfun = Class (Name "CasADi::SXFunction") methods
  where
    methods =
      [ Method (Name "SXFunction") (NewRef (Type "CasADi::SXFunction")) [vectorSXMat, vectorSXMat] (Const False) (Static True)
      , Method (Name "jac") (NewRef (Type "CasADi::SXMatrix")) [int, int] (Const False) (Static False)
      ]

tools :: [Function]
tools =
  [ Function (Name "CasADi::ssym") (NewRef (Type "CasADi::SXMatrix")) [stdStrRef, int, int]
  , Function (Name "CasADi::msym") (NewRef (Type "CasADi::MX")) [stdStrRef, int, int]
  ]

