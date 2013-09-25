{-# OPTIONS_GHC -Wall #-}

module CasadiTree ( fx, mx, sxfun, sxmat, tools ) where

import Types

cint :: Type
cint = Val (NonVec CInt)

vec :: Primitive -> ThreeVectors
vec x = Vec (NonVec x)

constSXMatVec :: Type
constSXMatVec = ConstRef (vec (CasadiClass SXMatrix))

constString :: Type
constString = ConstRef (vec StdString)

fx :: Class
fx = Class FX methods
  where
    methods =
      [ Method (Name "getNumScalarInputs") cint [] Normal
      ]

mx :: Class
mx = Class MX []

sxmat :: Class
sxmat = Class SXMatrix []


sxfun :: Class
sxfun = Class SXFunction methods
  where
    methods =
      [ Method (Name "SXFunction") (Val (NonVec (CasadiClass SXFunction))) [constSXMatVec, constSXMatVec] Constructor
      , Method (Name "jac") (Val (NonVec (CasadiClass SXMatrix))) [cint,cint] Normal
      ]

tools :: [Function]
tools =
  [ Function (Name "CasADi::ssym") (Val (NonVec (CasadiClass SXMatrix))) [constString, cint, cint]
  , Function (Name "CasADi::msym") (Val (NonVec (CasadiClass MX))) [constString, cint, cint]
  ]
