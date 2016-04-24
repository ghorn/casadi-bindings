{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}

module Casadi.Callback
       ( makeCallback
       ) where

import Data.Map ( Map )
import qualified Data.Traversable as T
import Data.Vector ( Vector )
import Foreign.Ptr ( Ptr, FunPtr )

import Casadi.Core.Data ( DM, DM', Function, Function'
                        , GenericType, GenericType', Sparsity, Sparsity' )
import Casadi.Internal.Marshal ( withMarshal )
import Casadi.Internal.MarshalTypes
import Casadi.Internal.WrapReturn ( WrapReturn(..) )

import Casadi.GenericType ( GType, toGType )

type HaskellCallback =
  Ptr (StdVec (Ptr DM')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr (StdVec DM))

foreign import ccall "wrapper" mkCallback
  :: HaskellCallback -> IO (FunPtr HaskellCallback)

foreign import ccall safe "new_callback_haskell" c_newCallbackHaskell
  :: FunPtr HaskellCallback
     -> Ptr (StdVec (Ptr Sparsity')) -> Ptr (StdVec (Ptr Sparsity'))
     -> IO (Ptr Function')

foreign import ccall safe "to_dm_vec" c_toDmVec
  :: Ptr (StdVec (Ptr DM')) -> IO (Ptr (StdVec DM))


-- | add a callback to an NLPSolver
makeCallback :: Vector Sparsity -> Vector Sparsity
             -> (Vector DM -> Map String GType -> IO (Vector DM)) -> IO Function
makeCallback sp_in sp_out userCallback = do
  -- safely wrap the callback into the C-friendly version
  let lowlevelCallback :: HaskellCallback
      lowlevelCallback dmInPtrs statsInPtrs = do
        dmIns <- wrapReturn dmInPtrs :: IO (Vector DM)
        stats0 <- wrapReturn statsInPtrs :: IO (Map String GenericType)
        stats <- T.mapM toGType stats0
        dmOuts <- userCallback dmIns stats :: IO (Vector DM)
        withMarshal dmOuts c_toDmVec

  -- turn the callback into a FunPtr
  callbackFunPtr <- mkCallback lowlevelCallback :: IO (FunPtr HaskellCallback)

  -- create the callback object
  withMarshal sp_in $ \sp_in' ->
    withMarshal sp_out $ \sp_out' ->
    c_newCallbackHaskell callbackFunPtr sp_in' sp_out' >>= wrapReturn
