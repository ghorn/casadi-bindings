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
import Casadi.DM ()
import Casadi.Viewable ( Viewable(..) )

instance Show SX where
  show x = unsafePerformIO (sx_getDescription x)
  {-# NOINLINE show #-}

instance Eq SX where
  x == y = unsafePerformIO (C.casadi_is_equal__2 x y)
  {-# NOINLINE (==) #-}

instance Conjugate SX where
  conjugate = id

instance Viewable SX where
  vvertcat = vertcat
  vvertsplit = vertsplit
  vsize1 = size1
  vsize2 = size2
  vrecoverDimension _ dim = zeros dim

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
  blocksplit x ix iy = unsafePerformIO (C.casadi_blocksplit__3 x ix iy)
  {-# NOINLINE blocksplit #-}
  blockcat x = unsafePerformIO (C.casadi_blockcat__1 x)
  {-# NOINLINE blockcat #-}
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
  mm x y = unsafePerformIO (C.casadi_mtimes__1 x y)
  {-# NOINLINE mm #-}
  dot x y = unsafePerformIO (C.casadi_dot__0 x y)
  {-# NOINLINE dot #-}
  sum1 x = unsafePerformIO (C.casadi_sum1__0 x)
  {-# NOINLINE sum1 #-}
  sum2 x = unsafePerformIO (C.casadi_sum2__0 x)
  {-# NOINLINE sum2 #-}
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
  indexed m spx spy = unsafePerformIO (sx_get__3 m False spx spy)
  {-# NOINLINE indexed #-}
  sparsity x = unsafePerformIO (sx_get_sparsity x)
  {-# NOINLINE sparsity #-}
  getNZ m sp = unsafePerformIO (sx_get_nz__1 m False sp)
  {-# NOINLINE getNZ #-}
  setNZ m y s = sx_set_nz__1 m y False s
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
  fromDM x = unsafePerformIO (sx__1 x)
  {-# NOINLINE fromDM #-}
  fromDVector x = fromDM (fromDVector x)
  {-# NOINLINE fromDVector #-}
  fromDouble x = unsafePerformIO (sx__5 x)
  {-# NOINLINE fromDouble #-}
  reshape x s = unsafePerformIO (C.casadi_reshape__1 x s)
  {-# NOINLINE reshape #-}
  conditional x0 x1 x2 = unsafePerformIO (C.casadi_conditional__0 x0 x1 x2)
  {-# NOINLINE conditional #-}
  conditional' x0 x1 x2 x3 = unsafePerformIO (C.casadi_conditional__1 x0 x1 x2 x3)
  {-# NOINLINE conditional' #-}
  inv x = unsafePerformIO (C.casadi_inv__0 x)
  {-# NOINLINE inv #-}
  pinv x = unsafePerformIO (C.casadi_pinv__2 x)
  {-# NOINLINE pinv #-}
  pinv' x n o = unsafePerformIO (C.casadi_pinv__1 x n o)
  {-# NOINLINE pinv' #-}
  cmax x y = unsafePerformIO (C.casadi_max__1 x y)
  {-# NOINLINE cmax #-}
  cmin x y = unsafePerformIO (C.casadi_min__1 x y)
  {-# NOINLINE cmin #-}
  cand x y = unsafePerformIO (C.casadi_and__1 x y)
  {-# NOINLINE cand #-}
  cor x y = unsafePerformIO (C.casadi_or__1 x y)
  {-# NOINLINE cor #-}
  repmat x (s1, s2) = unsafePerformIO (C.casadi_repmat__2 x s1 s2)
  {-# NOINLINE repmat #-}


instance Num SX where
  (+) x y = unsafePerformIO (C.casadi_plus__1 x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (C.casadi_minus__1 x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (C.casadi_times__1 x y)
  {-# NOINLINE (*) #-}
  fromInteger x = fromDouble (fromInteger x :: Double)
  {-# NOINLINE fromInteger #-}
  abs x = unsafePerformIO (C.casadi_abs__1 x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (C.casadi_sign__1 x)
  {-# NOINLINE signum #-}

instance Fractional SX where
  (/) x y = unsafePerformIO (C.casadi_rdivide__1 x y)
  {-# NOINLINE (/) #-}
  fromRational x = fromDouble (fromRational x :: Double)
  {-# NOINLINE fromRational #-}

instance Floating SX where
  pi = fromDouble (pi :: Double)
  {-# NOINLINE pi #-}
  (**) x y = unsafePerformIO (C.casadi_power__1 x y)
  {-# NOINLINE (**) #-}
  exp x   = unsafePerformIO (C.casadi_exp__1 x)
  {-# NOINLINE exp #-}
  log x   = unsafePerformIO (C.casadi_log__1 x)
  {-# NOINLINE log #-}
  sin x   = unsafePerformIO (C.casadi_sin__1 x)
  {-# NOINLINE sin #-}
  cos x   = unsafePerformIO (C.casadi_cos__1 x)
  {-# NOINLINE cos #-}
  tan x   = unsafePerformIO (C.casadi_tan__1 x)
  {-# NOINLINE tan #-}
  asin x  = unsafePerformIO (C.casadi_asin__1 x)
  {-# NOINLINE asin #-}
  atan x  = unsafePerformIO (C.casadi_atan__1 x)
  {-# NOINLINE atan #-}
  acos x  = unsafePerformIO (C.casadi_acos__1 x)
  {-# NOINLINE acos #-}
  sinh x  = unsafePerformIO (C.casadi_sinh__1 x)
  {-# NOINLINE sinh #-}
  cosh x  = unsafePerformIO (C.casadi_cosh__1 x)
  {-# NOINLINE cosh #-}
  tanh x  = unsafePerformIO (C.casadi_tanh__1 x)
  {-# NOINLINE tanh #-}
  asinh x = unsafePerformIO (C.casadi_asinh__1 x)
  {-# NOINLINE asinh #-}
  atanh x = unsafePerformIO (C.casadi_atanh__1 x)
  {-# NOINLINE atanh #-}
  acosh x = unsafePerformIO (C.casadi_acosh__1 x)
  {-# NOINLINE acosh #-}

instance Fmod SX where
  fmod x y = unsafePerformIO (C.casadi_mod__1 x y)
  {-# NOINLINE fmod #-}

instance ArcTan2 SX where
  arctan2 x y = unsafePerformIO (C.casadi_atan2__1 x y)
  {-# NOINLINE arctan2 #-}

instance SymOrd SX where
  x `leq` y = unsafePerformIO (C.casadi_le__1 x y)
  {-# NOINLINE leq #-}
  x `lt` y = unsafePerformIO (C.casadi_lt__1 x y)
  {-# NOINLINE lt #-}
  x `geq` y = unsafePerformIO (C.casadi_ge__1 x y)
  {-# NOINLINE geq #-}
  x `gt` y = unsafePerformIO (C.casadi_gt__1 x y)
  {-# NOINLINE gt #-}
  x  `eq` y = unsafePerformIO (C.casadi_eq__1 x y)
  {-# NOINLINE eq #-}
  max' x y = cmax x y
  {-# NOINLINE max' #-}
  min' x y = cmin x y
  {-# NOINLINE min' #-}

instance Erf SX where
  erf x = unsafePerformIO (C.casadi_erf__1 x)
  {-# NOINLINE erf #-}
  erfinv x = unsafePerformIO (C.casadi_erfinv__1 x)
  {-# NOINLINE erfinv #-}
