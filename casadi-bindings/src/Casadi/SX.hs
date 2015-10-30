{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.SX
       ( SX
       , ssym, ssymV, ssymM, sgradient, sjacobian, shessian
       , ssparsify
       ) where

import System.IO.Unsafe ( unsafePerformIO )
import Linear.Conjugate ( Conjugate(..) )

import Casadi.Core.Classes.SX
import qualified Casadi.Core.Tools as C

import Casadi.Overloading ( Fmod(..), ArcTan2(..), SymOrd(..), Erf(..) )
import Casadi.CMatrix ( CMatrix(..) )
import Casadi.DMatrix ()

instance Show SX where
  show x = unsafePerformIO (sx_getDescription x)
  {-# NOINLINE show #-}

instance Eq SX where
  x == y = unsafePerformIO (C.casadi_isEqual__0 x y)
  {-# NOINLINE (==) #-}

instance Conjugate SX where
  conjugate = id

ssym :: String -> IO SX
ssym = sx_sym__6

ssymV :: String -> Int -> IO SX
ssymV = sx_sym__7

ssymM :: String -> Int -> Int -> IO SX
ssymM = sx_sym__8

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
sgradient :: SX -> SX -> SX
sgradient x y = unsafePerformIO (C.casadi_gradient__0 x y)
{-# NOINLINE sgradient #-}

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
sjacobian :: SX -> SX -> SX
sjacobian x y = unsafePerformIO (C.casadi_jacobian__0 x y)
{-# NOINLINE sjacobian #-}

-- | @hessian exp x@ is the hessian of exp w.r.t. x
shessian :: SX -> SX -> SX -> SX
shessian x y z = unsafePerformIO (C.casadi_hessian__0 x y z)
{-# NOINLINE shessian #-}

ssparsify :: SX -> SX
ssparsify x = unsafePerformIO (C.casadi_sparsify__0 x)
{-# NOINLINE ssparsify #-}


instance CMatrix SX where
  veccat x = unsafePerformIO (C.casadi_veccat__0 x)
  {-# NOINLINE veccat #-}
  --  vertsplit = vertslice
  vertsplit x ks = unsafePerformIO (C.casadi_vertsplit__2 x ks)
  {-# NOINLINE vertsplit #-}
  vertcat x = unsafePerformIO (C.casadi_vertcat__0 x)
  {-# NOINLINE vertcat #-}
  --  horzsplit = horzslice
  horzsplit x ks = unsafePerformIO (C.casadi_horzsplit__2 x ks)
  {-# NOINLINE horzsplit #-}
  horzcat x = unsafePerformIO (C.casadi_horzcat__0 x)
  {-# NOINLINE horzcat #-}
  size1 x = unsafePerformIO (sx_size1 x)
  {-# NOINLINE size1 #-}
  size2 x = unsafePerformIO (sx_size2 x)
  {-# NOINLINE size2 #-}
  numel x = unsafePerformIO (sx_numel__1 x)
  {-# NOINLINE numel #-}
  mm x y = unsafePerformIO (C.casadi_mul__1 x y)
  {-# NOINLINE mm #-}
  innerProd x y = unsafePerformIO (C.casadi_inner_prod__0 x y)
  {-# NOINLINE innerProd #-}
  sumCols x = unsafePerformIO (C.casadi_sumCols__0 x)
  {-# NOINLINE sumCols #-}
  sumRows x = unsafePerformIO (C.casadi_sumRows__0 x)
  {-# NOINLINE sumRows #-}
  trans x = unsafePerformIO (sx_T x)
  {-# NOINLINE trans #-}
  diag x = unsafePerformIO (C.casadi_diag__0 x)
  {-# NOINLINE diag #-}
  eye n = unsafePerformIO (sx_eye n)
  {-# NOINLINE eye #-}
  ones (r,c) = unsafePerformIO (sx_ones__4 r c)
  {-# NOINLINE ones #-}
  zeros (r,c) = unsafePerformIO (sx_zeros__4 r c)
  {-# NOINLINE zeros #-}
  zerosSp sp = unsafePerformIO (sx_zeros__1 sp)
  {-# NOINLINE zerosSp #-}
  solve x y s m = unsafePerformIO (C.casadi_solve__1 x y s m)
  {-# NOINLINE solve #-}
  solve' x y = unsafePerformIO (C.casadi_solve__2 x y)
  {-# NOINLINE solve' #-}
  indexed m spx spy = unsafePerformIO $ do
    ret <- allocEmpty :: IO SX
    sx_get__3 m ret False spx spy
    return ret
  {-# NOINLINE indexed #-}
  sparsity x = unsafePerformIO (sx_getSparsity x)
  {-# NOINLINE sparsity #-}
  getNZ m sp = unsafePerformIO $ do
    ret <- allocEmpty :: IO SX
    sx_getNZ__1 m ret False sp
    return ret
  {-# NOINLINE getNZ #-}
  setNZ m y s = sx_setNZ__1 m y False s
  triu x = unsafePerformIO (C.casadi_triu__0 x)
  {-# NOINLINE triu #-}
  tril x = unsafePerformIO (C.casadi_tril__0 x)
  {-# NOINLINE tril #-}
  triu2symm x = unsafePerformIO (C.casadi_triu2symm__0 x)
  {-# NOINLINE triu2symm #-}
  tril2symm x = unsafePerformIO (C.casadi_tril2symm__0 x)
  {-# NOINLINE tril2symm #-}
  copy m = sx__9 m
  densify x = unsafePerformIO (C.casadi_densify__0 x)
  {-# NOINLINE densify #-}
  fromDMatrix x = unsafePerformIO (sx__1 x)
  {-# NOINLINE fromDMatrix #-}
  fromDVector x = fromDMatrix (fromDVector x)
  {-# NOINLINE fromDVector #-}
  fromDouble x = unsafePerformIO (sx__5 x)
  {-# NOINLINE fromDouble #-}
  allocEmpty = sx__10


instance Num SX where
  (+) x y = unsafePerformIO (C.casadi_plus__0 x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (C.casadi_minus__0 x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (C.casadi_times__0 x y)
  {-# NOINLINE (*) #-}
  fromInteger x = fromDouble (fromInteger x :: Double)
  {-# NOINLINE fromInteger #-}
  abs x = unsafePerformIO (C.casadi_abs__0 x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (C.casadi_sign__0 x)
  {-# NOINLINE signum #-}

instance Fractional SX where
  (/) x y = unsafePerformIO (C.casadi_rdivide__0 x y)
  {-# NOINLINE (/) #-}
  fromRational x = fromDouble (fromRational x :: Double)
  {-# NOINLINE fromRational #-}

instance Floating SX where
  pi = fromDouble (pi :: Double)
  {-# NOINLINE pi #-}
  (**) x y = unsafePerformIO (C.casadi_power__0 x y)
  {-# NOINLINE (**) #-}
  exp x   = unsafePerformIO (C.casadi_exp__0 x)
  {-# NOINLINE exp #-}
  log x   = unsafePerformIO (C.casadi_log__0 x)
  {-# NOINLINE log #-}
  sin x   = unsafePerformIO (C.casadi_sin__0 x)
  {-# NOINLINE sin #-}
  cos x   = unsafePerformIO (C.casadi_cos__0 x)
  {-# NOINLINE cos #-}
  tan x   = unsafePerformIO (C.casadi_tan__0 x)
  {-# NOINLINE tan #-}
  asin x  = unsafePerformIO (C.casadi_asin__0 x)
  {-# NOINLINE asin #-}
  atan x  = unsafePerformIO (C.casadi_atan__0 x)
  {-# NOINLINE atan #-}
  acos x  = unsafePerformIO (C.casadi_acos__0 x)
  {-# NOINLINE acos #-}
  sinh x  = unsafePerformIO (C.casadi_sinh__0 x)
  {-# NOINLINE sinh #-}
  cosh x  = unsafePerformIO (C.casadi_cosh__0 x)
  {-# NOINLINE cosh #-}
  tanh x  = unsafePerformIO (C.casadi_tanh__0 x)
  {-# NOINLINE tanh #-}
  asinh x = unsafePerformIO (C.casadi_asinh__0 x)
  {-# NOINLINE asinh #-}
  atanh x = unsafePerformIO (C.casadi_atanh__0 x)
  {-# NOINLINE atanh #-}
  acosh x = unsafePerformIO (C.casadi_acosh__0 x)
  {-# NOINLINE acosh #-}

instance Fmod SX where
  fmod x y = unsafePerformIO (C.casadi_mod__0 x y)
  {-# NOINLINE fmod #-}

instance ArcTan2 SX where
  arctan2 x y = unsafePerformIO (C.casadi_atan2__0 x y)
  {-# NOINLINE arctan2 #-}

instance SymOrd SX where
  x `leq` y = unsafePerformIO (C.casadi_le__0 x y)
  {-# NOINLINE leq #-}
  x `geq` y = unsafePerformIO (C.casadi_ge__0 x y)
  {-# NOINLINE geq #-}
  x  `eq` y = unsafePerformIO (C.casadi_eq__0 x y)
  {-# NOINLINE eq #-}

instance Erf SX where
  erf x = unsafePerformIO (C.casadi_erf__0 x)
  {-# NOINLINE erf #-}
  erfinv x = unsafePerformIO (C.casadi_erfinv__0 x)
  {-# NOINLINE erfinv #-}
