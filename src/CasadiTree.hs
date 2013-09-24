{-# OPTIONS_GHC -Wall #-}

module CasadiTree ( fx, mx, sxfun, sxmat, tools ) where

import Types

cint,constSXMatVec,constString :: Type
cint = Prim (SP CInt)
constSXMatVec = ConstRef (CP (Vec (CP SXMatrix)))
constString = ConstRef (SP StdString)

fx :: Class
fx = Class FX methods
  where
    methods =
      [ Method (Name "getNumScalarInputs") (SimpleType cint) [] (Const False) Normal
      ]

mx :: Class
mx = Class MX []

sxmat :: Class
sxmat = Class SXMatrix []


sxfun :: Class
sxfun = Class SXFunction methods
  where
    methods =
      [ Method (Name "SXFunction") (NewRef SXFunction) [constSXMatVec, constSXMatVec] (Const False) Constructor
      , Method (Name "jac") (NewRef SXMatrix) [cint,cint] (Const False) Normal
      ]

tools :: [Function]
tools =
  [ Function (Name "CasADi::ssym") (NewRef SXMatrix) [constString, cint, cint]
  , Function (Name "CasADi::msym") (NewRef MX) [constString, cint, cint]
  ]
