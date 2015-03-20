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
  x == y = unsafePerformIO (sx_zz_isEqual__0 x y)
  {-# NOINLINE (==) #-}

instance Conjugate SX where
  conjugate = id

ssym :: String -> IO SX
ssym = sx_sym__5

ssymV :: String -> Int -> IO SX
ssymV = sx_sym__6

ssymM :: String -> Int -> Int -> IO SX
ssymM = sx_sym__7

sfromDouble :: Double -> IO SX
sfromDouble = sx__5

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
sgradient :: SX -> SX -> SX
sgradient x y = unsafePerformIO (C.gradient__1 x y)
{-# NOINLINE sgradient #-}

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
sjacobian :: SX -> SX -> SX
sjacobian x y = unsafePerformIO (C.jacobian__1 x y)
{-# NOINLINE sjacobian #-}

-- | @hessian exp x@ is the hessian of exp w.r.t. x
shessian :: SX -> SX -> SX
shessian x y = unsafePerformIO (C.hessian__1 x y)
{-# NOINLINE shessian #-}

ssparsify :: SX -> SX
ssparsify x = unsafePerformIO (sx_zz_sparsify__0 x)
{-# NOINLINE ssparsify #-}


instance CMatrix SX where
  veccat x = unsafePerformIO (sx_zz_veccat x)
  {-# NOINLINE veccat #-}
  --  vertsplit = vertslice
  vertsplit x ks = unsafePerformIO (sx_zz_vertsplit x ks)
  {-# NOINLINE vertsplit #-}
  vertcat x = unsafePerformIO (sx_zz_vertcat x)
  {-# NOINLINE vertcat #-}
  --  horzsplit = horzslice
  horzsplit x ks = unsafePerformIO (sx_zz_horzsplit x ks)
  {-# NOINLINE horzsplit #-}
  horzcat x = unsafePerformIO (sx_zz_horzcat x)
  {-# NOINLINE horzcat #-}
  size1 x = unsafePerformIO (sx_size1 x)
  {-# NOINLINE size1 #-}
  size2 x = unsafePerformIO (sx_size2 x)
  {-# NOINLINE size2 #-}
  numel x = unsafePerformIO (sx_numel__1 x)
  {-# NOINLINE numel #-}
  mm x y = unsafePerformIO (sx_zz_mtimes__1 x y)
  {-# NOINLINE mm #-}
  innerProd x y = unsafePerformIO (sx_zz_inner_prod x y)
  {-# NOINLINE innerProd #-}
  trans x = unsafePerformIO (sx_T x)
  {-# NOINLINE trans #-}
  diag x = unsafePerformIO (sx_zz_diag x)
  {-# NOINLINE diag #-}
  eye n = unsafePerformIO (sx_eye n)
  {-# NOINLINE eye #-}
  ones (r,c) = unsafePerformIO (sx_ones__3 r c)
  {-# NOINLINE ones #-}
  zeros (r,c) = unsafePerformIO (sx_zeros__3 r c)
  {-# NOINLINE zeros #-}
  zerosSp sp = unsafePerformIO (sx_zeros__0 sp)
  {-# NOINLINE zerosSp #-}
  solve a b = unsafePerformIO (C.solve__2 a b)
  {-# NOINLINE solve #-}
  indexed m spx spy = unsafePerformIO $ do
    ret <- allocEmpty :: IO SX
    sx_get__3 m ret False spx spy
    return ret
  {-# NOINLINE indexed #-}
  sparsity x = unsafePerformIO (sx_sparsityRef x)
  {-# NOINLINE sparsity #-}
  getNZ m sp = unsafePerformIO $ do
    ret <- allocEmpty :: IO SX
    sx_getNZ__1 m ret False sp
    return ret
  {-# NOINLINE getNZ #-}
  setNZ m y s = sx_setNZ__1 m y False s
  triu x = unsafePerformIO (sx_zz_triu__0 x)
  {-# NOINLINE triu #-}
  tril x = unsafePerformIO (sx_zz_tril__0 x)
  {-# NOINLINE tril #-}
  triu2symm x = unsafePerformIO (sx_zz_triu2symm x)
  {-# NOINLINE triu2symm #-}
  tril2symm x = unsafePerformIO (sx_zz_tril2symm x)
  {-# NOINLINE tril2symm #-}
  copy m = sx__9 m
  densify x = unsafePerformIO (sx_zz_densify x)
  {-# NOINLINE densify #-}
  fromDMatrix x = unsafePerformIO (sx__1 x)
  {-# NOINLINE fromDMatrix #-}
  fromDVector x = fromDMatrix (fromDVector x)
  {-# NOINLINE fromDVector #-}
  allocEmpty = sx__10


instance Num SX where
  (+) x y = unsafePerformIO (sx_zz_plus x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (sx_zz_minus x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (sx_zz_times x y)
  {-# NOINLINE (*) #-}
  fromInteger x = unsafePerformIO (sfromDouble (fromInteger x :: Double))
  {-# NOINLINE fromInteger #-}
  abs x = unsafePerformIO (sx_zz_abs x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (sx_zz_sign x)
  {-# NOINLINE signum #-}

instance Fractional SX where
  (/) x y = unsafePerformIO (sx___truediv____0 x y)
  {-# NOINLINE (/) #-}
  fromRational x = unsafePerformIO (sfromDouble (fromRational x :: Double))
  {-# NOINLINE fromRational #-}

instance Floating SX where
  pi = unsafePerformIO (sfromDouble (pi :: Double))
  {-# NOINLINE pi #-}
  (**) x y = unsafePerformIO (sx_zz_power x y)
  {-# NOINLINE (**) #-}
  exp x   = unsafePerformIO (sx_zz_exp x)
  {-# NOINLINE exp #-}
  log x   = unsafePerformIO (sx_zz_log x)
  {-# NOINLINE log #-}
  sin x   = unsafePerformIO (sx_zz_sin x)
  {-# NOINLINE sin #-}
  cos x   = unsafePerformIO (sx_zz_cos x)
  {-# NOINLINE cos #-}
  tan x   = unsafePerformIO (sx_zz_tan x)
  {-# NOINLINE tan #-}
  asin x  = unsafePerformIO (sx_zz_asin x)
  {-# NOINLINE asin #-}
  atan x  = unsafePerformIO (sx_zz_atan x)
  {-# NOINLINE atan #-}
  acos x  = unsafePerformIO (sx_zz_acos x)
  {-# NOINLINE acos #-}
  sinh x  = unsafePerformIO (sx_zz_sinh x)
  {-# NOINLINE sinh #-}
  cosh x  = unsafePerformIO (sx_zz_cosh x)
  {-# NOINLINE cosh #-}
  tanh x  = unsafePerformIO (sx_zz_tanh x)
  {-# NOINLINE tanh #-}
  asinh x = unsafePerformIO (sx_zz_asinh x)
  {-# NOINLINE asinh #-}
  atanh x = unsafePerformIO (sx_zz_atanh x)
  {-# NOINLINE atanh #-}
  acosh x = unsafePerformIO (sx_zz_acosh x)
  {-# NOINLINE acosh #-}

instance Fmod SX where
  fmod x y = unsafePerformIO (sx_zz_mod x y)
  {-# NOINLINE fmod #-}

instance ArcTan2 SX where
  arctan2 x y = unsafePerformIO (sx_zz_atan2 x y)
  {-# NOINLINE arctan2 #-}

instance SymOrd SX where
  x `leq` y = unsafePerformIO (sx_zz_le x y)
  {-# NOINLINE leq #-}
  x `geq` y = unsafePerformIO (sx_zz_ge x y)
  {-# NOINLINE geq #-}
  x  `eq` y = unsafePerformIO (sx_zz_eq x y)
  {-# NOINLINE eq #-}

instance Erf SX where
  erf x = unsafePerformIO (sx_zz_erf x)
  {-# NOINLINE erf #-}
  erfinv x = unsafePerformIO (sx_zz_erfinv x)
  {-# NOINLINE erfinv #-}
