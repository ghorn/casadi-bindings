{-# OPTIONS_GHC -Wall #-}

module CasadiTree ( fx, mx, sxfun, sxmat, sxmatvec, tools ) where

import Types

fx :: Class
fx = Class FX methods
  where
    methods =
      [ Method (Name "getNumScalarInputs") (SimpleType CInt) [] (Const False) (Static False)
      ]

mx :: Class
mx = Class MX []

sxmat :: Class
sxmat = Class SXMatrix []

sxmatvec :: Class
sxmatvec = Class SXMatrixVector []

sxfun :: Class
sxfun = Class SXFunction methods
  where
    methods =
      [ Method (Name "SXFunction") (NewRef SXFunction) [Ref SXMatrixVector, Ref SXMatrixVector] (Const False) (Static True)
      , Method (Name "jac") (NewRef SXMatrix) [CInt, CInt] (Const False) (Static False)
      ]

tools :: [Function]
tools =
  [ Function (Name "CasADi::ssym") (NewRef SXMatrix) [Ref StdString, CInt, CInt]
  , Function (Name "CasADi::msym") (NewRef MX) [Ref StdString, CInt, CInt]
  ]

