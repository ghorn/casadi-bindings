{-# OPTIONS_GHC -Wall #-}

module Casadi.Matrix
       ( CMatrix(..), SMatrix(..)
       , vertslice, horzslice
       ) where

import qualified Data.Map as M
import qualified Data.Vector as V

import Casadi.Core.Classes.DM ( DM )
import Casadi.Core.Classes.Function ( Function )

import Casadi.GenericType ( GType )
import Casadi.Overloading ( Fmod, ArcTan2, SymOrd, Erf )
import Casadi.Sparsity ( Sparsity )
import Casadi.Slice ( Slice, slice )
import Casadi.Viewable ( Viewable )

-- TODO(greg): alphabetize this, it's getting too big to manage
-- | casadi matrix
class (Eq a, Show a, Floating a, Fmod a, ArcTan2 a, SymOrd a, Erf a, Viewable a)
      => CMatrix a where
  blocksplit :: a -> V.Vector Int -> V.Vector Int -> V.Vector (V.Vector a)
  blockcat :: V.Vector (V.Vector a) -> a
  vertsplit :: a -> V.Vector Int -> V.Vector a
  vertcat :: V.Vector a -> a
  horzsplit :: a -> V.Vector Int -> V.Vector a
  horzcat :: V.Vector a -> a
  veccat :: V.Vector a -> a
  size1 :: a -> Int
  size2 :: a -> Int
  numel :: a -> Int
  -- | matrix matrix product
  mtimes :: a -> a -> a
  -- | sumAll(x*y), x and y same dimension
  dot :: a -> a -> a
  sum1 :: a -> a
  sum2 :: a -> a
  sumSquare :: a -> a
  -- | transpose
  trans :: a -> a
  diag :: a -> a
  eye :: Int -> a
  ones :: (Int,Int) -> a
  zeros :: (Int,Int) -> a
  zerosSp :: Sparsity -> a
  solve :: a -> a -> String -> M.Map String GType -> a
  solve' :: a -> a -> a
  indexed :: a -> Slice -> Slice -> a
  sparsity :: a -> Sparsity
  getNZ :: a -> Slice -> a
  setNZ :: a -> a -> Slice -> IO ()
  inv :: a -> a
  invSkew :: a -> a
  pinv :: a -> a
  pinv' :: a -> String -> M.Map String GType -> a
  triu :: a -> a
  tril :: a -> a
  triu2symm :: a -> a
  tril2symm :: a -> a
  copy :: a -> IO a
  densify :: a -> a
  fromDM :: DM -> a
  fromDVector :: V.Vector Double -> a
  fromDouble :: Double -> a
  reshape :: a -> (Int, Int) -> a
  conditional :: a -> V.Vector a -> a -> a
  conditional' :: a -> V.Vector a -> a -> Bool -> a
  cmin :: a -> a -> a
  cmax :: a -> a -> a
  cand :: a -> a -> a
  cor :: a -> a -> a
  cnot :: a -> a
  nullspace :: a -> a
  norm1 :: a -> a
  norm2 :: a -> a
  normFro :: a -> a
  normInf :: a -> a

--  if_else ::
  -- TODO(greg): any and all, mac, repsum?
  repmat :: a -> (Int, Int) -> a
  printme :: a -> a -> a
  kron :: a -> a -> a
  mldivide :: a -> a -> a
  mrdivide :: a -> a -> a
  mpower :: a -> a -> a
--  matrix_expand :: a -> a -> a

  ceil' :: a -> a
  floor' :: a -> a


-- | symbolic matrix
class CMatrix a => SMatrix a where
  sym :: String -> Int -> Int -> IO a
  toFunction :: String -> V.Vector a -> V.Vector a -> M.Map String GType -> IO Function
  toFunction' :: String -> V.Vector (String, a) -> V.Vector (String, a) -> M.Map String GType -> IO Function
  -- | @gradient exp x@ is the gradient of exp w.r.t. x
  gradient :: a -> a -> a
  -- | @jacobian exp x@ is the jacobian of exp w.r.t. x
  jacobian :: a -> a -> a
  -- | @(hess, grad) = hessian exp args@
  hessian :: a -> a -> (a, a)
  jtimes :: a -> a -> a -> a
  forward :: V.Vector a -> V.Vector a -> V.Vector (V.Vector a) -> M.Map String GType -> V.Vector (V.Vector a)
  reverse :: V.Vector a -> V.Vector a -> V.Vector (V.Vector a) -> M.Map String GType -> V.Vector (V.Vector a)

  callSym :: Function -> V.Vector a -> V.Vector a
  callSym' :: Function -> M.Map String a -> M.Map String a


vertslice :: CMatrix a => a -> V.Vector Int -> V.Vector a
vertslice x vs = V.fromList (f (V.toList vs))
  where
    cols = size2 x
    hslice = slice 0 cols 1

    f (v0:v1:others) = indexed x (slice v0 v1 1) hslice : f (v1:others)
    f _ = []

horzslice :: CMatrix a => a -> V.Vector Int -> V.Vector a
horzslice x vs = V.fromList (f (V.toList vs))
  where
    rows = size1 x
    vslice = slice 0 rows 1

    f (v0:v1:others) = indexed x vslice (slice v0 v1 1) : f (v1:others)
    f _ = []
