{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.MX
       ( MX
       , sym, symV, symM, gradient, jacobian, hessian
       , expand
       ) where

import Data.Vector ( Vector )
import System.IO.Unsafe ( unsafePerformIO )
import Linear.Conjugate ( Conjugate(..) )

import Casadi.Core.Classes.MX
import qualified Casadi.Core.Tools as C

import Casadi.Overloading ( Fmod(..), ArcTan2(..), SymOrd(..), Erf(..) )
import Casadi.CMatrix ( CMatrix(..) )
import Casadi.DM ()
import Casadi.Viewable ( Viewable(..) )
import Casadi.SharedObject ( castSharedObject )

instance Conjugate MX where
  conjugate = id

instance Eq MX where
  x == y = unsafePerformIO (C.casadi_is_equal__8 x y)
  {-# NOINLINE (==) #-}

instance Show MX where
  show x = show (castSharedObject x)
  {-# NOINLINE show #-}

instance Viewable MX where
  vvertcat = vertcat
  vvertsplit = vertsplit
  vsize1 = size1
  vsize2 = size2
  vrecoverDimension _ dim = zeros dim

sym :: String -> IO MX
sym x = fmap castMX (mx_sym__6 x)

symV :: String -> Int -> IO MX
symV x y = fmap castMX (mx_sym__7 x y)

symM :: String -> Int -> Int -> IO MX
symM x y z = fmap castMX (mx_sym__8 x y z)

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
gradient :: MX -> MX -> MX
gradient x y = unsafePerformIO (C.casadi_gradient__3 x y)
{-# NOINLINE gradient #-}

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
jacobian :: MX -> MX -> MX
jacobian x y = unsafePerformIO (C.casadi_jacobian__3 x y)
{-# NOINLINE jacobian #-}

expand :: Vector MX -> Vector MX
expand x = unsafePerformIO (C.casadi_matrix_expand__3 x)
{-# NOINLINE expand #-}

---- | @hessian exp x@ is the jacobian of exp w.r.t. x
hessian :: MX -> MX -> MX -> MX
hessian x y z = unsafePerformIO (C.casadi_hessian__3 x y z)
{-# NOINLINE hessian #-}

--sparsify :: MX -> MX
--sparsify x = unsafePerformIO (C.casadi_sparsify__0__3 x)
--{-# NOINLINE sparsify #-}

instance CMatrix MX where
  blocksplit x ix iy = unsafePerformIO (C.casadi_blocksplit__15 x ix iy)
  {-# NOINLINE blocksplit #-}
  blockcat x = unsafePerformIO (C.casadi_blockcat__7 x)
  {-# NOINLINE blockcat #-}
  veccat x = unsafePerformIO (C.casadi_veccat__3 x)
  {-# NOINLINE veccat #-}
  --  vertsplit = vertslice
  vertsplit x ks = unsafePerformIO (C.casadi_vertsplit__11 x ks)
  {-# NOINLINE vertsplit #-}
  vertcat x = unsafePerformIO (C.casadi_vertcat__3 x)
  {-# NOINLINE vertcat #-}
  --  horzsplit = horzslice
  horzsplit x ks = unsafePerformIO (C.casadi_horzsplit__11 x ks)
  {-# NOINLINE horzsplit #-}
  horzcat x = unsafePerformIO (C.casadi_horzcat__3 x)
  {-# NOINLINE horzcat #-}
  size1 x = unsafePerformIO (mx_size1 x)
  {-# NOINLINE size1 #-}
  size2 x = unsafePerformIO (mx_size2 x)
  {-# NOINLINE size2 #-}
  numel x = unsafePerformIO (mx_numel__1 x)
  {-# NOINLINE numel #-}
  mm x y = unsafePerformIO (C.casadi_mtimes__7 x y)
  {-# NOINLINE mm #-}
  dot x y = unsafePerformIO (C.casadi_dot__3 x y)
  {-# NOINLINE dot #-}
  sum1 x = unsafePerformIO (C.casadi_sum1__3 x)
  {-# NOINLINE sum1 #-}
  sum2 x = unsafePerformIO (C.casadi_sum2__3 x)
  {-# NOINLINE sum2 #-}
  trans x = unsafePerformIO (mx_T x)
  {-# NOINLINE trans #-}
  diag x = unsafePerformIO (C.casadi_diag__3 x)
  {-# NOINLINE diag #-}
  eye n = unsafePerformIO (mx_eye n)
  {-# NOINLINE eye #-}
  ones (r,c) = unsafePerformIO (mx_ones__4 r c)
  {-# NOINLINE ones #-}
  zeros (r,c) = unsafePerformIO (mx_zeros__4 r c)
  {-# NOINLINE zeros #-}
  zerosSp sp = unsafePerformIO (mx_zeros__1 sp)
  {-# NOINLINE zerosSp #-}
  solve x y s m = unsafePerformIO (C.casadi_solve__10 x y s m)
  {-# NOINLINE solve #-}
  solve' x y = unsafePerformIO (C.casadi_solve__11 x y)
  {-# NOINLINE solve' #-}
  indexed m spx spy = unsafePerformIO (mx_get__3 m False spx spy)
  {-# NOINLINE indexed #-}
  sparsity x = unsafePerformIO (mx_get_sparsity x)
  {-# NOINLINE sparsity #-}
  getNZ m sp = unsafePerformIO (mx_get_nz__1 m False sp)
  {-# NOINLINE getNZ #-}
  setNZ m y s = mx_set_nz__1 m y False s
  triu x = unsafePerformIO (C.casadi_triu__6 x)
  {-# NOINLINE triu #-}
  tril x = unsafePerformIO (C.casadi_tril__6 x)
  {-# NOINLINE tril #-}
  triu2symm x = unsafePerformIO (C.casadi_triu2symm__3 x)
  {-# NOINLINE triu2symm #-}
  tril2symm x = unsafePerformIO (C.casadi_tril2symm__3 x)
  {-# NOINLINE tril2symm #-}
  copy m = mx__2 m
  densify x = unsafePerformIO (C.casadi_densify__3 x)
  {-# NOINLINE densify #-}
  fromDM x = unsafePerformIO (mx__0 x)
  {-# NOINLINE fromDM #-}
  fromDVector x = fromDM (fromDVector x)
  {-# NOINLINE fromDVector #-}
  fromDouble x = unsafePerformIO (mx__3 x)
  {-# NOINLINE fromDouble #-}
  reshape x s = unsafePerformIO (C.casadi_reshape__10 x s)
  {-# NOINLINE reshape #-}
  conditional x0 x1 x2 = unsafePerformIO (C.casadi_conditional__6 x0 x1 x2)
  {-# NOINLINE conditional #-}
  conditional' x0 x1 x2 x3 = unsafePerformIO (C.casadi_conditional__7 x0 x1 x2 x3)
  {-# NOINLINE conditional' #-}
  inv x = unsafePerformIO (C.casadi_inv__3 x)
  {-# NOINLINE inv #-}
  pinv x = unsafePerformIO (C.casadi_pinv__11 x)
  {-# NOINLINE pinv #-}
  pinv' x n o = unsafePerformIO (C.casadi_pinv__10 x n o)
  {-# NOINLINE pinv' #-}
  cmax x y = unsafePerformIO (C.casadi_max__4 x y)
  {-# NOINLINE cmax #-}
  cmin x y = unsafePerformIO (C.casadi_min__4 x y)
  {-# NOINLINE cmin #-}


instance Num MX where
  (+) x y = unsafePerformIO (C.casadi_plus__4 x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (C.casadi_minus__4 x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (C.casadi_times__4 x y)
  {-# NOINLINE (*) #-}
  fromInteger x = fromDouble (fromInteger x :: Double)
  {-# NOINLINE fromInteger #-}
  abs x = unsafePerformIO (C.casadi_abs__4 x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (C.casadi_sign__4 x)
  {-# NOINLINE signum #-}

instance Fractional MX where
  (/) x y = unsafePerformIO (C.casadi_rdivide__4 x y)
  {-# NOINLINE (/) #-}
  fromRational x = fromDouble (fromRational x :: Double)
  {-# NOINLINE fromRational #-}

instance Floating MX where
  pi = fromDouble (pi :: Double)
  {-# NOINLINE pi #-}
  (**) x y = unsafePerformIO (C.casadi_power__4 x y)
  {-# NOINLINE (**) #-}
  exp x   = unsafePerformIO (C.casadi_exp__4 x)
  {-# NOINLINE exp #-}
  log x   = unsafePerformIO (C.casadi_log__4 x)
  {-# NOINLINE log #-}
  sin x   = unsafePerformIO (C.casadi_sin__4 x)
  {-# NOINLINE sin #-}
  cos x   = unsafePerformIO (C.casadi_cos__4 x)
  {-# NOINLINE cos #-}
  tan x   = unsafePerformIO (C.casadi_tan__4 x)
  {-# NOINLINE tan #-}
  asin x  = unsafePerformIO (C.casadi_asin__4 x)
  {-# NOINLINE asin #-}
  atan x  = unsafePerformIO (C.casadi_atan__4 x)
  {-# NOINLINE atan #-}
  acos x  = unsafePerformIO (C.casadi_acos__4 x)
  {-# NOINLINE acos #-}
  sinh x  = unsafePerformIO (C.casadi_sinh__4 x)
  {-# NOINLINE sinh #-}
  cosh x  = unsafePerformIO (C.casadi_cosh__4 x)
  {-# NOINLINE cosh #-}
  tanh x  = unsafePerformIO (C.casadi_tanh__4 x)
  {-# NOINLINE tanh #-}
  asinh x = unsafePerformIO (C.casadi_asinh__4 x)
  {-# NOINLINE asinh #-}
  atanh x = unsafePerformIO (C.casadi_atanh__4 x)
  {-# NOINLINE atanh #-}
  acosh x = unsafePerformIO (C.casadi_acosh__4 x)
  {-# NOINLINE acosh #-}

instance Fmod MX where
  fmod x y = unsafePerformIO (C.casadi_mod__4 x y)
  {-# NOINLINE fmod #-}

instance ArcTan2 MX where
  arctan2 x y = unsafePerformIO (C.casadi_atan2__4 x y)
  {-# NOINLINE arctan2 #-}

instance SymOrd MX where
  x `leq` y = unsafePerformIO (C.casadi_le__4 x y)
  {-# NOINLINE leq #-}
  x `geq` y = unsafePerformIO (C.casadi_ge__4 x y)
  {-# NOINLINE geq #-}
  x  `eq` y = unsafePerformIO (C.casadi_eq__4 x y)
  {-# NOINLINE eq #-}

instance Erf MX where
  erf x = unsafePerformIO (C.casadi_erf__4 x)
  {-# NOINLINE erf #-}
  erfinv x = unsafePerformIO (C.casadi_erfinv__4 x)
  {-# NOINLINE erfinv #-}
