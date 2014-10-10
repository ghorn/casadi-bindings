{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.SXElement
       ( SXElement(), sxElement_sym
       ) where

--import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )
import Linear.Conjugate ( Conjugate(..) )

import Casadi.Core.Classes.SXElement

--import Casadi.SX ( svector )
import Casadi.Overloading ( Fmod(..), ArcTan2(..), SymOrd(..) )

--instance Show SXElement where
--  show = show . svector . V.singleton

instance Conjugate SXElement where
  conjugate = id

instance Num SXElement where
  (+) x y = unsafePerformIO (sxElement___add__ x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (sxElement___sub__ x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (sxElement___mul__ x y)
  {-# NOINLINE (*) #-}
  abs x = unsafePerformIO (sxElement_fabs x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (sxElement_sign x)
  {-# NOINLINE signum #-}
  fromInteger x = unsafePerformIO (sxElement__0 (fromInteger x :: Double))
  {-# NOINLINE fromInteger #-}

instance Fractional SXElement where
  (/) x y = unsafePerformIO (sxElement___truediv____0 x y)
  {-# NOINLINE (/) #-}
  fromRational x = unsafePerformIO (sxElement__0 (fromRational x :: Double))
  {-# NOINLINE fromRational #-}

instance Floating SXElement where
  pi = unsafePerformIO (sxElement__0 (pi :: Double))
  {-# NOINLINE pi #-}
  (**) x y = unsafePerformIO (sxElement___pow__ x y)
  {-# NOINLINE (**) #-}
  exp x   = unsafePerformIO (sxElement_exp x)
  {-# NOINLINE exp #-}
  log x   = unsafePerformIO (sxElement_log x)
  {-# NOINLINE log #-}
  sin x   = unsafePerformIO (sxElement_sin x)
  {-# NOINLINE sin #-}
  cos x   = unsafePerformIO (sxElement_cos x)
  {-# NOINLINE cos #-}
  tan x   = unsafePerformIO (sxElement_tan x)
  {-# NOINLINE tan #-}
  asin x  = unsafePerformIO (sxElement_arcsin x)
  {-# NOINLINE asin #-}
  atan x  = unsafePerformIO (sxElement_arctan x)
  {-# NOINLINE atan #-}
  acos x  = unsafePerformIO (sxElement_arccos x)
  {-# NOINLINE acos #-}
  sinh x  = unsafePerformIO (sxElement_sinh x)
  {-# NOINLINE sinh #-}
  cosh x  = unsafePerformIO (sxElement_cosh x)
  {-# NOINLINE cosh #-}
  tanh x  = unsafePerformIO (sxElement_tanh x)
  {-# NOINLINE tanh #-}
  asinh x = unsafePerformIO (sxElement_arcsinh x)
  {-# NOINLINE asinh #-}
  atanh x = unsafePerformIO (sxElement_arctanh x)
  {-# NOINLINE atanh #-}
  acosh x = unsafePerformIO (sxElement_arccosh x)
  {-# NOINLINE acosh #-}

instance Fmod SXElement where
  fmod x y = unsafePerformIO (sxElement_fmod x y)
  {-# NOINLINE fmod #-}

instance ArcTan2 SXElement where
  arctan2 x y = unsafePerformIO (sxElement_arctan2__1 x y)
  {-# NOINLINE arctan2 #-}

instance SymOrd SXElement where
  x `leq` y = unsafePerformIO (sxElement___le__ x y)
  {-# NOINLINE leq #-}
  x `geq` y = unsafePerformIO (sxElement___ge__ x y)
  {-# NOINLINE geq #-}
  x  `eq` y = unsafePerformIO (sxElement___eq__ x y)
  {-# NOINLINE eq #-}
