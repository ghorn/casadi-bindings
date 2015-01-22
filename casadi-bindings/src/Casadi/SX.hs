{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.SX
       ( SX, ssym, ssymV, ssymM, smm, strans
       , sgradient, sjacobian, shessian, svector, sdiag
       , ssolve
       , sdata
       , striu
       , stril
       , sdense, ssparsify
       , d2s
       , ssize, ssize1, ssize2, snumel
       , scrs, svertcat, shorzcat, sveccat, svertsplit, shorzsplit
       , seye, sones, szeros
       , sindexed
       ) where

import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )
import Linear.Conjugate ( Conjugate(..) )

import Casadi.Core.Classes.SXElement ( SXElement )
import Casadi.Core.Classes.SX
import Casadi.Core.Classes.DMatrix ( DMatrix )
import Casadi.Core.Classes.Sparsity ( Sparsity )
import Casadi.Core.Classes.Slice ( Slice )
import qualified Casadi.Core.Tools as C

import Casadi.Overloading ( Fmod(..), ArcTan2(..), SymOrd(..) )

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

-- | matrix matrix product
smm :: SX -> SX -> SX
smm x y = unsafePerformIO (sx_zz_mtimes__1 x y)
{-# NOINLINE smm #-}

d2s :: DMatrix -> SX
d2s x = unsafePerformIO (sx__2 x)
{-# NOINLINE d2s #-}

sdiag :: SX -> SX
sdiag x = unsafePerformIO (sx_zz_diag x)
{-# NOINLINE sdiag #-}

-- | transpose
strans :: SX -> SX
strans x = unsafePerformIO (sx_T x)
{-# NOINLINE strans #-}

sdense :: SX -> SX
sdense x = unsafePerformIO (sx_zz_dense x)
{-# NOINLINE sdense #-}

ssparsify :: SX -> SX
ssparsify x = unsafePerformIO (sx_zz_sparsify__0 x)
{-# NOINLINE ssparsify #-}

striu :: SX -> SX
striu x = unsafePerformIO (sx_zz_triu__0 x)
{-# NOINLINE striu #-}

stril :: SX -> SX
stril x = unsafePerformIO (sx_zz_tril__0 x)
{-# NOINLINE stril #-}

scrs :: SX -> Sparsity
scrs x = unsafePerformIO (sx_sparsityRef__0 x)
{-# NOINLINE scrs #-}

-- | from SXElement vector
svector :: V.Vector SXElement -> SX
svector x = unsafePerformIO (sx__7 x)
{-# NOINLINE svector #-}

sdata :: SX -> V.Vector SXElement
sdata x = unsafePerformIO (sx_data__0 x)
{-# NOINLINE sdata #-}

ssize :: SX -> Int
ssize x = unsafePerformIO (sx_size__1 x)
{-# NOINLINE ssize #-}

ssize1 :: SX -> Int
ssize1 x = unsafePerformIO (sx_size1 x)
{-# NOINLINE ssize1 #-}

ssize2 :: SX -> Int
ssize2 x = unsafePerformIO (sx_size2 x)
{-# NOINLINE ssize2 #-}

snumel :: SX -> Int
snumel x = unsafePerformIO (sx_numel__1 x)
{-# NOINLINE snumel #-}

svertcat :: V.Vector SX -> SX
svertcat x = unsafePerformIO (sx_zz_vertcat x)
{-# NOINLINE svertcat #-}

shorzcat :: V.Vector SX -> SX
shorzcat x = unsafePerformIO (sx_zz_horzcat x)
{-# NOINLINE shorzcat #-}

sveccat :: V.Vector SX -> SX
sveccat x = unsafePerformIO (sx_zz_veccat x)
{-# NOINLINE sveccat #-}

svertsplit :: SX -> V.Vector Int -> V.Vector SX
svertsplit x ks = unsafePerformIO (sx_zz_vertsplit x ks)
{-# NOINLINE svertsplit #-}

shorzsplit :: SX -> V.Vector Int -> V.Vector SX
shorzsplit x ks = unsafePerformIO (sx_zz_horzsplit x ks)
{-# NOINLINE shorzsplit #-}

ssolve :: SX -> SX -> SX
ssolve a b = unsafePerformIO (C.solve__2 a b)
{-# NOINLINE ssolve #-}

seye :: Int -> SX
seye n = unsafePerformIO (sx_eye n)
{-# NOINLINE seye #-}

sones :: (Int,Int) -> SX
sones (r,c) = unsafePerformIO (sx_ones__3 r c)
{-# NOINLINE sones #-}

szeros :: (Int,Int) -> SX
szeros (r,c) = unsafePerformIO (sx_zeros__3 r c)
{-# NOINLINE szeros #-}

sindexed :: SX -> Slice -> Slice -> SX
sindexed m sx sy = unsafePerformIO (sx_getSub__3 m False sx sy)
{-# NOINLINE sindexed #-}

instance Num SX where
  (+) x y = unsafePerformIO (sx_zz_plus x y)
  {-# NOINLINE (+) #-}
  (-) x y = unsafePerformIO (sx_zz_minus x y)
  {-# NOINLINE (-) #-}
  (*) x y = unsafePerformIO (sx_zz_times x y)
  {-# NOINLINE (*) #-}
  fromInteger x = unsafePerformIO (sx__8 (fromInteger x :: Double))
  {-# NOINLINE fromInteger #-}
  abs x = unsafePerformIO (sx_zz_abs x)
  {-# NOINLINE abs #-}
  signum x = unsafePerformIO (sx_zz_sign x)
  {-# NOINLINE signum #-}

instance Fractional SX where
  (/) x y = unsafePerformIO (sx___truediv____0 x y)
  {-# NOINLINE (/) #-}
  fromRational x = unsafePerformIO (sx__8 (fromRational x :: Double))
  {-# NOINLINE fromRational #-}

instance Floating SX where
  pi = unsafePerformIO (sx__8 (pi :: Double))
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
