{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}

module Casadi.Callback
       ( makeCallback
       ) where

import Data.Map ( Map )
import qualified Data.Traversable as T
import Data.Vector ( Vector )
import Foreign.ForeignPtr ( newForeignPtr )
import Foreign.Ptr ( Ptr, FunPtr )

import Casadi.Core.Data ( DM, DM', Function(..), Function'
                        , GenericType, GenericType', Sparsity, Sparsity' )
import Casadi.Internal.Marshal ( withMarshal )
import Casadi.Internal.MarshalTypes
import Casadi.Internal.WrapReturn ( WrapReturn(..) )

import Casadi.GenericType ( GType, toGType )

type HaskellCallbackFun =
  Ptr (StdVec (Ptr DM')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr (StdVec DM))

foreign import ccall "wrapper" mkCallback
  :: HaskellCallbackFun -> IO (FunPtr HaskellCallbackFun)

foreign import ccall safe "new_haskell_callback" c_newHaskellCallback
  :: Ptr StdString
  -> FunPtr HaskellCallbackFun
  -> Ptr (StdVec (Ptr Sparsity'))
  -> Ptr (StdVec (Ptr Sparsity'))
  -> IO (Ptr Function')

foreign import ccall safe "&delete_haskell_callback" c_deleteHaskellCallback
  :: FunPtr (Ptr Function' -> IO ())

foreign import ccall safe "to_dm_vec" c_toDmVec
  :: Ptr (StdVec (Ptr DM')) -> IO (Ptr (StdVec DM))

-- | add a callback to an NLPSolver
makeCallback :: String -> Vector Sparsity -> Vector Sparsity
             -> (Vector DM -> Map String GType -> IO (Vector DM)) -> IO Function
makeCallback name sp_in sp_out userCallback = do
  -- safely wrap the callback into the C-friendly version
  let lowlevelCallback :: HaskellCallbackFun
      lowlevelCallback dmInPtrs statsInPtrs = do
        dmIns <- wrapReturn dmInPtrs :: IO (Vector DM)
        stats0 <- wrapReturn statsInPtrs :: IO (Map String GenericType)
        stats <- T.mapM toGType stats0
        dmOuts <- userCallback dmIns stats :: IO (Vector DM)
        withMarshal dmOuts c_toDmVec

  -- turn the callback into a FunPtr
  callbackFunPtr <- mkCallback lowlevelCallback :: IO (FunPtr HaskellCallbackFun)

  -- create the callback object
  withMarshal name $ \name' ->
    withMarshal sp_in $ \sp_in' ->
    withMarshal sp_out $ \sp_out' -> do
    cbFun <- c_newHaskellCallback name' callbackFunPtr sp_in' sp_out' :: IO (Ptr Function')
    Function <$> newForeignPtr c_deleteHaskellCallback cbFun
