{-# OPTIONS_GHC -Wall #-}

module CasadiTree ( fx, sxfun, tools ) where

import Types

fx :: Class
fx = Class FX methods
  where
    methods =
      [ Method (Name "getNumScalarInputs") (SimpleType CInt) [] (Const False) (Static False)
      ]

sxfun :: Class
sxfun = Class SXFunction methods
  where
    methods =
      [ Method (Name "SXFunction") (NewRef SXFunction) [Ref VectorSXMat, Ref VectorSXMat] (Const False) (Static True)
      , Method (Name "jac") (NewRef SXMatrix) [CInt, CInt] (Const False) (Static False)
      ]

tools :: [Function]
tools =
  [ Function (Name "CasADi::ssym") (NewRef SXMatrix) [StdStr, CInt, CInt]
  , Function (Name "CasADi::msym") (NewRef MX) [Ref StdStr, CInt, CInt]
  ]

