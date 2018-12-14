{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.Callback
       (
         Callback,
         CallbackClass(..),
         callback__0,
         callback__1,
         callback_alloc_arg__0,
         callback_alloc_arg__1,
         callback_alloc_iw__0,
         callback_alloc_iw__1,
         callback_alloc_res__0,
         callback_alloc_res__1,
         callback_alloc_w__0,
         callback_alloc_w__1,
         callback_construct__0,
         callback_construct__1,
         callback_eval,
         callback_finalize,
         callback_get_forward,
         callback_get_jacobian,
         callback_get_n_in,
         callback_get_n_out,
         callback_get_name_in,
         callback_get_name_out,
         callback_get_reverse,
         callback_get_sparsity_in,
         callback_get_sparsity_out,
         callback_has_forward,
         callback_has_jacobian,
         callback_has_reverse,
         callback_init,
         callback_type_name,
         callback_uses_output,
       ) where


import Prelude hiding ( Functor )

import Data.Vector ( Vector )
import qualified Data.Map as M
import Foreign.C.Types
import Foreign.Marshal ( new, free )
import Foreign.Storable ( peek )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.ForeignPtr ( newForeignPtr )
import System.IO.Unsafe ( unsafePerformIO ) -- for show instances

import Casadi.Internal.FormatException ( formatException )
import Casadi.Internal.MarshalTypes ( StdVec, StdString, StdMap, StdPair ) -- StdPair StdOstream'
import Casadi.Internal.Marshal ( Marshal(..), marshal, marshalFree )
import Casadi.Internal.WrapReturn ( WrapReturn(..) )
import Casadi.Core.Data
import Casadi.Core.Enums
-- direct wrapper
foreign import ccall unsafe "casadi__Callback__CONSTRUCTOR__0" c_casadi__Callback__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> Ptr Callback' -> IO (Ptr Callback')

casadi__Callback__CONSTRUCTOR__0
  :: Callback -> IO Callback
casadi__Callback__CONSTRUCTOR__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__CONSTRUCTOR__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
callback__0 :: Callback -> IO Callback
callback__0 = casadi__Callback__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__CONSTRUCTOR__1" c_casadi__Callback__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> IO (Ptr Callback')

casadi__Callback__CONSTRUCTOR__1
  :: IO Callback
casadi__Callback__CONSTRUCTOR__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__CONSTRUCTOR__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
callback__1 :: IO Callback
callback__1 = casadi__Callback__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__alloc_arg__0" c_casadi__Callback__alloc_arg__0
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CSize -> IO ()

casadi__Callback__alloc_arg__0
  :: Callback -> CSize -> IO ()
casadi__Callback__alloc_arg__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__alloc_arg__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
callback_alloc_arg__0 :: CallbackClass a => a -> CSize -> IO ()
callback_alloc_arg__0 x = casadi__Callback__alloc_arg__0 (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__alloc_arg__1" c_casadi__Callback__alloc_arg__1
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CSize -> CInt -> IO ()

casadi__Callback__alloc_arg__1
  :: Callback -> CSize -> Bool -> IO ()
casadi__Callback__alloc_arg__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__alloc_arg__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
callback_alloc_arg__1 :: CallbackClass a => a -> CSize -> Bool -> IO ()
callback_alloc_arg__1 x = casadi__Callback__alloc_arg__1 (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__alloc_iw__0" c_casadi__Callback__alloc_iw__0
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CSize -> IO ()

casadi__Callback__alloc_iw__0
  :: Callback -> CSize -> IO ()
casadi__Callback__alloc_iw__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__alloc_iw__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
callback_alloc_iw__0 :: CallbackClass a => a -> CSize -> IO ()
callback_alloc_iw__0 x = casadi__Callback__alloc_iw__0 (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__alloc_iw__1" c_casadi__Callback__alloc_iw__1
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CSize -> CInt -> IO ()

casadi__Callback__alloc_iw__1
  :: Callback -> CSize -> Bool -> IO ()
casadi__Callback__alloc_iw__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__alloc_iw__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
callback_alloc_iw__1 :: CallbackClass a => a -> CSize -> Bool -> IO ()
callback_alloc_iw__1 x = casadi__Callback__alloc_iw__1 (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__alloc_res__0" c_casadi__Callback__alloc_res__0
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CSize -> IO ()

casadi__Callback__alloc_res__0
  :: Callback -> CSize -> IO ()
casadi__Callback__alloc_res__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__alloc_res__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
callback_alloc_res__0 :: CallbackClass a => a -> CSize -> IO ()
callback_alloc_res__0 x = casadi__Callback__alloc_res__0 (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__alloc_res__1" c_casadi__Callback__alloc_res__1
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CSize -> CInt -> IO ()

casadi__Callback__alloc_res__1
  :: Callback -> CSize -> Bool -> IO ()
casadi__Callback__alloc_res__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__alloc_res__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
callback_alloc_res__1 :: CallbackClass a => a -> CSize -> Bool -> IO ()
callback_alloc_res__1 x = casadi__Callback__alloc_res__1 (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__alloc_w__0" c_casadi__Callback__alloc_w__0
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CSize -> IO ()

casadi__Callback__alloc_w__0
  :: Callback -> CSize -> IO ()
casadi__Callback__alloc_w__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__alloc_w__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
callback_alloc_w__0 :: CallbackClass a => a -> CSize -> IO ()
callback_alloc_w__0 x = casadi__Callback__alloc_w__0 (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__alloc_w__1" c_casadi__Callback__alloc_w__1
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CSize -> CInt -> IO ()

casadi__Callback__alloc_w__1
  :: Callback -> CSize -> Bool -> IO ()
casadi__Callback__alloc_w__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__alloc_w__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
callback_alloc_w__1 :: CallbackClass a => a -> CSize -> Bool -> IO ()
callback_alloc_w__1 x = casadi__Callback__alloc_w__1 (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__construct__0" c_casadi__Callback__construct__0
  :: Ptr (Ptr StdString) -> Ptr Callback' -> Ptr StdString -> IO ()

casadi__Callback__construct__0
  :: Callback -> String -> IO ()
casadi__Callback__construct__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__construct__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
callback_construct__0 :: CallbackClass a => a -> String -> IO ()
callback_construct__0 x = casadi__Callback__construct__0 (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__construct__1" c_casadi__Callback__construct__1
  :: Ptr (Ptr StdString) -> Ptr Callback' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO ()

casadi__Callback__construct__1
  :: Callback -> String -> M.Map String GenericType -> IO ()
casadi__Callback__construct__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__construct__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
callback_construct__1 :: CallbackClass a => a -> String -> M.Map String GenericType -> IO ()
callback_construct__1 x = casadi__Callback__construct__1 (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__eval" c_casadi__Callback__eval
  :: Ptr (Ptr StdString) -> Ptr Callback' -> Ptr (StdVec (Ptr DM')) -> IO (Ptr (StdVec (Ptr DM')))

casadi__Callback__eval
  :: Callback -> Vector DM -> IO (Vector DM)
casadi__Callback__eval x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__eval errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
callback_eval :: CallbackClass a => a -> Vector DM -> IO (Vector DM)
callback_eval x = casadi__Callback__eval (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__finalize" c_casadi__Callback__finalize
  :: Ptr (Ptr StdString) -> Ptr Callback' -> IO ()

casadi__Callback__finalize
  :: Callback -> IO ()
casadi__Callback__finalize x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__finalize errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
callback_finalize :: CallbackClass a => a -> IO ()
callback_finalize x = casadi__Callback__finalize (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__get_forward" c_casadi__Callback__get_forward
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CLLong -> Ptr StdString -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Callback__get_forward
  :: Callback -> Int -> String -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
casadi__Callback__get_forward x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__get_forward errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ret



-- classy wrapper
callback_get_forward :: CallbackClass a => a -> Int -> String -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
callback_get_forward x = casadi__Callback__get_forward (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__get_jacobian" c_casadi__Callback__get_jacobian
  :: Ptr (Ptr StdString) -> Ptr Callback' -> Ptr StdString -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Callback__get_jacobian
  :: Callback -> String -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
casadi__Callback__get_jacobian x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__get_jacobian errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ret



-- classy wrapper
callback_get_jacobian :: CallbackClass a => a -> String -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
callback_get_jacobian x = casadi__Callback__get_jacobian (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__get_n_in" c_casadi__Callback__get_n_in
  :: Ptr (Ptr StdString) -> Ptr Callback' -> IO CLLong

casadi__Callback__get_n_in
  :: Callback -> IO Int
casadi__Callback__get_n_in x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__get_n_in errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
callback_get_n_in :: CallbackClass a => a -> IO Int
callback_get_n_in x = casadi__Callback__get_n_in (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__get_n_out" c_casadi__Callback__get_n_out
  :: Ptr (Ptr StdString) -> Ptr Callback' -> IO CLLong

casadi__Callback__get_n_out
  :: Callback -> IO Int
casadi__Callback__get_n_out x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__get_n_out errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
callback_get_n_out :: CallbackClass a => a -> IO Int
callback_get_n_out x = casadi__Callback__get_n_out (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__get_name_in" c_casadi__Callback__get_name_in
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CLLong -> IO (Ptr StdString)

casadi__Callback__get_name_in
  :: Callback -> Int -> IO String
casadi__Callback__get_name_in x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__get_name_in errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
callback_get_name_in :: CallbackClass a => a -> Int -> IO String
callback_get_name_in x = casadi__Callback__get_name_in (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__get_name_out" c_casadi__Callback__get_name_out
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CLLong -> IO (Ptr StdString)

casadi__Callback__get_name_out
  :: Callback -> Int -> IO String
casadi__Callback__get_name_out x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__get_name_out errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
callback_get_name_out :: CallbackClass a => a -> Int -> IO String
callback_get_name_out x = casadi__Callback__get_name_out (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__get_reverse" c_casadi__Callback__get_reverse
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CLLong -> Ptr StdString -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Callback__get_reverse
  :: Callback -> Int -> String -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
casadi__Callback__get_reverse x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__get_reverse errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ret



-- classy wrapper
callback_get_reverse :: CallbackClass a => a -> Int -> String -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
callback_get_reverse x = casadi__Callback__get_reverse (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__get_sparsity_in" c_casadi__Callback__get_sparsity_in
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CLLong -> IO (Ptr Sparsity')

casadi__Callback__get_sparsity_in
  :: Callback -> Int -> IO Sparsity
casadi__Callback__get_sparsity_in x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__get_sparsity_in errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
callback_get_sparsity_in :: CallbackClass a => a -> Int -> IO Sparsity
callback_get_sparsity_in x = casadi__Callback__get_sparsity_in (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__get_sparsity_out" c_casadi__Callback__get_sparsity_out
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CLLong -> IO (Ptr Sparsity')

casadi__Callback__get_sparsity_out
  :: Callback -> Int -> IO Sparsity
casadi__Callback__get_sparsity_out x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__get_sparsity_out errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
callback_get_sparsity_out :: CallbackClass a => a -> Int -> IO Sparsity
callback_get_sparsity_out x = casadi__Callback__get_sparsity_out (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__has_forward" c_casadi__Callback__has_forward
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CLLong -> IO CInt

casadi__Callback__has_forward
  :: Callback -> Int -> IO Bool
casadi__Callback__has_forward x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__has_forward errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
callback_has_forward :: CallbackClass a => a -> Int -> IO Bool
callback_has_forward x = casadi__Callback__has_forward (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__has_jacobian" c_casadi__Callback__has_jacobian
  :: Ptr (Ptr StdString) -> Ptr Callback' -> IO CInt

casadi__Callback__has_jacobian
  :: Callback -> IO Bool
casadi__Callback__has_jacobian x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__has_jacobian errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
callback_has_jacobian :: CallbackClass a => a -> IO Bool
callback_has_jacobian x = casadi__Callback__has_jacobian (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__has_reverse" c_casadi__Callback__has_reverse
  :: Ptr (Ptr StdString) -> Ptr Callback' -> CLLong -> IO CInt

casadi__Callback__has_reverse
  :: Callback -> Int -> IO Bool
casadi__Callback__has_reverse x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__has_reverse errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
callback_has_reverse :: CallbackClass a => a -> Int -> IO Bool
callback_has_reverse x = casadi__Callback__has_reverse (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__init" c_casadi__Callback__init
  :: Ptr (Ptr StdString) -> Ptr Callback' -> IO ()

casadi__Callback__init
  :: Callback -> IO ()
casadi__Callback__init x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__init errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
callback_init :: CallbackClass a => a -> IO ()
callback_init x = casadi__Callback__init (castCallback x)


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__type_name" c_casadi__Callback__type_name
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__Callback__type_name
  :: IO String
casadi__Callback__type_name  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__type_name errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
callback_type_name :: IO String
callback_type_name = casadi__Callback__type_name


-- direct wrapper
foreign import ccall unsafe "casadi__Callback__uses_output" c_casadi__Callback__uses_output
  :: Ptr (Ptr StdString) -> Ptr Callback' -> IO CInt

casadi__Callback__uses_output
  :: Callback -> IO Bool
casadi__Callback__uses_output x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Callback__uses_output errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
callback_uses_output :: CallbackClass a => a -> IO Bool
callback_uses_output x = casadi__Callback__uses_output (castCallback x)

