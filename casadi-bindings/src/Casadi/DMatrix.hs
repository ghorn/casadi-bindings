{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.DMatrix
       ( DMatrix
       , dsparsify
       , dnonzeros
       ) where

import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )
import Linear.Conjugate ( Conjugate(..) )
import qualified Data.Serialize as S
import qualified Data.Binary as B
import Data.Vector.Binary () -- instances

import Casadi.Core.Classes.DMatrix
import Casadi.Core.Classes.Sparsity ( Sparsity )
import qualified Casadi.Core.Tools as C

import Casadi.Overloading ( Fmod(..), ArcTan2(..), SymOrd(..), Erf(..) )
import Casadi.CMatrix ( CMatrix(..) )
import Casadi.Viewable ( Viewable(..) )

getWith :: Monad m => m Sparsity -> m (V.Vector Double) -> m DMatrix
getWith get getVector = do
  sp <- get
  data' <- getVector
  return (fromSparseData sp data')
putWith :: Monad m => (Sparsity -> m ()) -> (V.Vector Double -> m ()) -> DMatrix -> m ()
putWith put putVector x = do
  put (sparsity x)
  putVector (dnonzeros x)

-- Data.Vector.Cereal looks deprecated, it's not in master anymore
instance S.Serialize DMatrix where
  put = putWith S.put (S.put . V.toList)
  get = getWith S.get (fmap V.fromList S.get)
instance B.Binary DMatrix where
  put = putWith B.put B.put
  get = getWith B.get B.get

instance Conjugate DMatrix where
  conjugate = id

instance Viewable DMatrix where
  vvertcat = vertcat
  vvertsplit = vertsplit
  vsize1 = size1
  vsize2 = size2
  vrecoverDimension _ dim = zeros dim

fromSparseData :: Sparsity -> V.Vector Double -> DMatrix
fromSparseData s d = unsafePerformIO (dmatrix__4 s (fromDVector d))
{-# NOINLINE fromSparseData #-}

dsparsify :: DMatrix -> DMatrix
dsparsify x = unsafePerformIO (C.casadi_sparsify__2 x)
{-# NOINLINE dsparsify #-}

dnonzeros :: DMatrix -> V.Vector Double
dnonzeros x = unsafePerformIO (dmatrix_nonzeros x)
{-# NOINLINE dnonzeros #-}

instance Show DMatrix where
  show x = unsafePerformIO (dmatrix_getDescription x)
  {-# NOINLINE show #-}

instance Eq DMatrix where
  x == y = unsafePerformIO (C.casadi_isEqual__2 x y)
  {-# NOINLINE (==) #-}

instance CMatrix DMatrix where
  blocksplit x ix iy = unsafePerformIO (C.casadi_blocksplit__7 x ix iy)
  {-# NOINLINE blocksplit #-}
  blockcat x = unsafePerformIO (C.casadi_blockcat__3 x)
  {-# NOINLINE blockcat #-}
  veccat x = unsafePerformIO (C.casadi_veccat__1 x)
  {-# NOINLINE veccat #-}
  ----  vertsplit = vertslice
  vertsplit x ks = unsafePerformIO (C.casadi_vertsplit__5 x ks)
  {-# NOINLINE vertsplit #-}
  vertcat x = unsafePerformIO (C.casadi_vertcat__1 x)
  {-# NOINLINE vertcat #-}
  --  horzsplit = horzslice
  horzsplit x ks = unsafePerformIO (C.casadi_horzsplit__5 x ks)
  {-# NOINLINE horzsplit #-}
  horzcat x = unsafePerformIO (C.casadi_horzcat__1 x)
  {-# NOINLINE horzcat #-}
  size1 x = unsafePerformIO (dmatrix_size1 x)
  {-# NOINLINE size1 #-}
  size2 x = unsafePerformIO (dmatrix_size2 x)
  {-# NOINLINE size2 #-}
  numel x = unsafePerformIO (dmatrix_numel__1 x)
  {-# NOINLINE numel #-}
  mm x y = unsafePerformIO (C.casadi_mul__3 x y)
  {-# NOINLINE mm #-}
  innerProd x y = unsafePerformIO (C.casadi_inner_prod__1 x y)
  {-# NOINLINE innerProd #-}
  sumCols x = unsafePerformIO (C.casadi_sumCols__1 x)
  {-# NOINLINE sumCols #-}
  sumRows x = unsafePerformIO (C.casadi_sumRows__1 x)
  {-# NOINLINE sumRows #-}
  trans x = unsafePerformIO (dmatrix_T x)
  {-# NOINLINE trans #-}
  diag x = unsafePerformIO (C.casadi_diag__1 x)
  {-# NOINLINE diag #-}
  eye n = unsafePerformIO (dmatrix_eye n)
  {-# NOINLINE eye #-}
  ones (r,c) = unsafePerformIO (dmatrix_ones__4 r c)
  {-# NOINLINE ones #-}
  zeros (r,c) = unsafePerformIO (dmatrix_zeros__4 r c)
  {-# NOINLINE zeros #-}
  zerosSp sp = unsafePerformIO (dmatrix_zeros__1 sp)
  {-# NOINLINE zerosSp #-}
  solve x y s m = unsafePerformIO (C.casadi_solve__4 x y s m)
  {-# NOINLINE solve #-}
  solve' x y = unsafePerformIO (C.casadi_solve__5 x y)
  {-# NOINLINE solve' #-}
  indexed m spx spy = unsafePerformIO $ do
    ret <- allocEmpty :: IO DMatrix
    dmatrix_get__3 m ret False spx spy
    return ret
  {-# NOINLINE indexed #-}
  sparsity x = unsafePerformIO (dmatrix_getSparsity x)
  {-# NOINLINE sparsity #-}
  getNZ m sp = unsafePerformIO $ do
    ret <- allocEmpty :: IO DMatrix
    dmatrix_getNZ__1 m ret False sp
    return ret
  {-# NOINLINE getNZ #-}
  setNZ m y s = dmatrix_setNZ__1 m y False s
  triu x = unsafePerformIO (C.casadi_triu__2 x)
  {-# NOINLINE triu #-}
  tril x = unsafePerformIO (C.casadi_tril__2 x)
  {-# NOINLINE tril #-}
  triu2symm x = unsafePerformIO (C.casadi_triu2symm__1 x)
  {-# NOINLINE triu2symm #-}
  tril2symm x = unsafePerformIO (C.casadi_tril2symm__1 x)
  {-# NOINLINE tril2symm #-}
  copy m = dmatrix__7 m
  densify x = unsafePerformIO (C.casadi_densify__1 x)
  {-# NOINLINE densify #-}
  fromDMatrix = id
  fromDVector x = unsafePerformIO (dmatrix__2 (V.singleton x) >>= dmatrix_T)
  {-# NOINLINE fromDVector #-}
  fromDouble x = unsafePerformIO (dmatrix__3 x)
  {-# NOINLINE fromDouble #-}
  allocEmpty = dmatrix__8


instance Num DMatrix where
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

instance Fractional DMatrix where
  (/) x y = unsafePerformIO (C.casadi_rdivide__1 x y)
  {-# NOINLINE (/) #-}
  fromRational x = fromDouble (fromRational x :: Double)
  {-# NOINLINE fromRational #-}

instance Floating DMatrix where
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

instance Fmod DMatrix where
  fmod x y = unsafePerformIO (C.casadi_mod__1 x y)
  {-# NOINLINE fmod #-}

instance ArcTan2 DMatrix where
  arctan2 x y = unsafePerformIO (C.casadi_atan2__1 x y)
  {-# NOINLINE arctan2 #-}

instance SymOrd DMatrix where
  x `leq` y = unsafePerformIO (C.casadi_le__1 x y)
  {-# NOINLINE leq #-}
  x `geq` y = unsafePerformIO (C.casadi_ge__1 x y)
  {-# NOINLINE geq #-}
  x  `eq` y = unsafePerformIO (C.casadi_eq__1 x y)
  {-# NOINLINE eq #-}

instance Erf DMatrix where
  erf x = unsafePerformIO (C.casadi_erf__1 x)
  {-# NOINLINE erf #-}
  erfinv x = unsafePerformIO (C.casadi_erfinv__1 x)
  {-# NOINLINE erfinv #-}
