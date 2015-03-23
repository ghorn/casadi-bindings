{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.MX
       ( MX
       , sym, symV, symM, gradient, jacobian -- , hessian
       , expand
       ) where

import Data.Vector ( Vector )
import System.IO.Unsafe ( unsafePerformIO )
import Linear.Conjugate ( Conjugate(..) )

import Casadi.Core.Classes.MX
import qualified Casadi.Core.Tools as C

import Casadi.Overloading ( Fmod(..), ArcTan2(..), SymOrd(..), Erf(..) )
import Casadi.CMatrix ( CMatrix(..) )
import Casadi.DMatrix ()
import Casadi.SharedObject ( castSharedObject )

instance Conjugate MX where
  conjugate = id

instance Eq MX where
  x == y = unsafePerformIO (mx_zz_isEqual__0 x y)
  {-# NOINLINE (==) #-}

instance Show MX where
  show x = show (castSharedObject x)
  {-# NOINLINE show #-}

sym :: String -> IO MX
sym x = fmap castMX (mx_sym__5 x)

symV :: String -> Int -> IO MX
symV x y = fmap castMX (mx_sym__6 x y)

symM :: String -> Int -> Int -> IO MX
symM x y z = fmap castMX (mx_sym__7 x y z)

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
gradient :: MX -> MX -> MX
gradient x y = unsafePerformIO (C.gradient__0 x y)
{-# NOINLINE gradient #-}

-- | @jacobian exp x@ is the jacobian of exp w.r.t. x
jacobian :: MX -> MX -> MX
jacobian x y = unsafePerformIO (C.jacobian__0 x y)
{-# NOINLINE jacobian #-}

expand :: Vector MX -> Vector MX
expand x = unsafePerformIO (C.matrix_expand__0 x)
{-# NOINLINE expand #-}

---- | @hessian exp x@ is the jacobian of exp w.r.t. x
--hessian :: MX -> MX -> MX
--hessian x y = unsafePerformIO (C.hessian x y)
--{-# NOINLINE hessian #-}

--sparsify :: MX -> MX
--sparsify x = unsafePerformIO (mx_zz_sparsify__0 x)
--{-# NOINLINE sparsify #-}

instance CMatrix MX where
  veccat x = unsafePerformIO (mx_zz_veccat x)
  {-# NOINLINE veccat #-}
  --  vertsplit = vertslice
  vertsplit x ks = unsafePerformIO (mx_zz_vertsplit x ks)
  {-# NOINLINE vertsplit #-}
  vertcat x = unsafePerformIO (mx_zz_vertcat x)
  {-# NOINLINE vertcat #-}
  --  horzsplit = horzslice
  horzsplit x ks = unsafePerformIO (mx_zz_horzsplit x ks)
  {-# NOINLINE horzsplit #-}
  horzcat x = unsafePerformIO (mx_zz_horzcat x)
  {-# NOINLINE horzcat #-}
  size1 x = unsafePerformIO (mx_size1 x)
  {-# NOINLINE size1 #-}
  size2 x = unsafePerformIO (mx_size2 x)
  {-# NOINLINE size2 #-}
  numel x = unsafePerformIO (mx_numel__1 x)
  {-# NOINLINE numel #-}
  mm x y = unsafePerformIO (mx_zz_mtimes__1 x y)
  {-# NOINLINE mm #-}
  innerProd x y = unsafePerformIO (mx_zz_inner_prod x y)
  {-# NOINLINE innerProd #-}
  trans x = unsafePerformIO (mx_T x)
  {-# NOINLINE trans #-}
  diag x = unsafePerformIO (mx_zz_diag x)
  {-# NOINLINE diag #-}
  eye n = unsafePerformIO (mx_eye n)
  {-# NOINLINE eye #-}
  ones (r,c) = unsafePerformIO (mx_ones__3 r c)
  {-# NOINLINE ones #-}
  zeros (r,c) = unsafePerformIO (mx_zeros__3 r c)
  {-# NOINLINE zeros #-}
  zerosSp sp = unsafePerformIO (mx_zeros__0 sp)
  {-# NOINLINE zerosSp #-}
  solve a b = unsafePerformIO (C.solve__0 a b)
  {-# NOINLINE solve #-}
  indexed m spx spy = unsafePerformIO $ do
    ret <- allocEmpty :: IO MX
    mx_get__3 m ret False spx spy
    return ret
  {-# NOINLINE indexed #-}
  sparsity x = unsafePerformIO (mx_sparsityRef x)
  {-# NOINLINE sparsity #-}
  getNZ m sp = unsafePerformIO $ do
    ret <- allocEmpty :: IO MX
    mx_getNZ__1 m ret False sp
    return ret
  {-# NOINLINE getNZ #-}
  setNZ m y s = mx_setNZ__1 m y False s
  triu x = unsafePerformIO (mx_zz_triu__0 x)
  {-# NOINLINE triu #-}
  tril x = unsafePerformIO (mx_zz_tril__0 x)
  {-# NOINLINE tril #-}
  triu2symm x = unsafePerformIO (mx_zz_triu2symm x)
  {-# NOINLINE triu2symm #-}
  tril2symm x = unsafePerformIO (mx_zz_tril2symm x)
  {-# NOINLINE tril2symm #-}
  copy m = mx__2 m
  densify x = unsafePerformIO (mx_zz_densify x)
  {-# NOINLINE densify #-}
  fromDMatrix x = unsafePerformIO (mx__0 x)
  {-# NOINLINE fromDMatrix #-}
  fromDVector x = fromDMatrix (fromDVector x)
  {-# NOINLINE fromDVector #-}
  fromDouble x = unsafePerformIO (mx__3 x)
  {-# NOINLINE fromDouble #-}
  allocEmpty = mx__7


instance Num MX where
  (+) x y = unsafePerformIO (mx_zz_plus x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (mx_zz_minus x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (mx_zz_times x y)
  {-# NOINLINE (*) #-}
  fromInteger x = fromDouble (fromInteger x :: Double)
  {-# NOINLINE fromInteger #-}
  abs x = unsafePerformIO (mx_zz_abs x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (mx_zz_sign x)
  {-# NOINLINE signum #-}

instance Fractional MX where
  (/) x y = unsafePerformIO (mx___truediv____0 x y)
  {-# NOINLINE (/) #-}
  fromRational x = fromDouble (fromRational x :: Double)
  {-# NOINLINE fromRational #-}

instance Floating MX where
  pi = fromDouble (pi :: Double)
  {-# NOINLINE pi #-}
  (**) x y = unsafePerformIO (mx_zz_power x y)
  {-# NOINLINE (**) #-}
  exp x   = unsafePerformIO (mx_zz_exp x)
  {-# NOINLINE exp #-}
  log x   = unsafePerformIO (mx_zz_log x)
  {-# NOINLINE log #-}
  sin x   = unsafePerformIO (mx_zz_sin x)
  {-# NOINLINE sin #-}
  cos x   = unsafePerformIO (mx_zz_cos x)
  {-# NOINLINE cos #-}
  tan x   = unsafePerformIO (mx_zz_tan x)
  {-# NOINLINE tan #-}
  asin x  = unsafePerformIO (mx_zz_asin x)
  {-# NOINLINE asin #-}
  atan x  = unsafePerformIO (mx_zz_atan x)
  {-# NOINLINE atan #-}
  acos x  = unsafePerformIO (mx_zz_acos x)
  {-# NOINLINE acos #-}
  sinh x  = unsafePerformIO (mx_zz_sinh x)
  {-# NOINLINE sinh #-}
  cosh x  = unsafePerformIO (mx_zz_cosh x)
  {-# NOINLINE cosh #-}
  tanh x  = unsafePerformIO (mx_zz_tanh x)
  {-# NOINLINE tanh #-}
  asinh x = unsafePerformIO (mx_zz_asinh x)
  {-# NOINLINE asinh #-}
  atanh x = unsafePerformIO (mx_zz_atanh x)
  {-# NOINLINE atanh #-}
  acosh x = unsafePerformIO (mx_zz_acosh x)
  {-# NOINLINE acosh #-}

instance Fmod MX where
  fmod x y = unsafePerformIO (mx_zz_mod x y)
  {-# NOINLINE fmod #-}

instance ArcTan2 MX where
  arctan2 x y = unsafePerformIO (mx_zz_atan2 x y)
  {-# NOINLINE arctan2 #-}

instance SymOrd MX where
  x `leq` y = unsafePerformIO (mx_zz_le x y)
  {-# NOINLINE leq #-}
  x `geq` y = unsafePerformIO (mx_zz_ge x y)
  {-# NOINLINE geq #-}
  x  `eq` y = unsafePerformIO (mx_zz_eq x y)
  {-# NOINLINE eq #-}

instance Erf MX where
  erf x = unsafePerformIO (mx_zz_erf x)
  {-# NOINLINE erf #-}
  erfinv x = unsafePerformIO (mx_zz_erfinv x)
  {-# NOINLINE erfinv #-}
