{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.DaeBuilder
       (
         DaeBuilder,
         DaeBuilderClass(..),
         daeBuilder,
         daeBuilder_add_alg,
         daeBuilder_add_aux__0,
         daeBuilder_add_aux__1,
         daeBuilder_add_aux__2,
         daeBuilder_add_d,
         daeBuilder_add_dae,
         daeBuilder_add_fun__0,
         daeBuilder_add_fun__1,
         daeBuilder_add_fun__2,
         daeBuilder_add_fun__3,
         daeBuilder_add_fun__4,
         daeBuilder_add_lc,
         daeBuilder_add_ode,
         daeBuilder_add_p__0,
         daeBuilder_add_p__1,
         daeBuilder_add_p__2,
         daeBuilder_add_q__0,
         daeBuilder_add_q__1,
         daeBuilder_add_q__2,
         daeBuilder_add_quad,
         daeBuilder_add_s__0,
         daeBuilder_add_s__1,
         daeBuilder_add_s__2,
         daeBuilder_add_u__0,
         daeBuilder_add_u__1,
         daeBuilder_add_u__2,
         daeBuilder_add_variable__0,
         daeBuilder_add_variable__1,
         daeBuilder_add_variable__2,
         daeBuilder_add_variable__3,
         daeBuilder_add_x__0,
         daeBuilder_add_x__1,
         daeBuilder_add_x__2,
         daeBuilder_add_y,
         daeBuilder_add_z__0,
         daeBuilder_add_z__1,
         daeBuilder_add_z__2,
         daeBuilder_create,
         daeBuilder_der__0,
         daeBuilder_der__1,
         daeBuilder_derivative_start__0,
         daeBuilder_derivative_start__1,
         daeBuilder_derivative_start__2,
         daeBuilder_derivative_start__3,
         daeBuilder_eliminate_alg,
         daeBuilder_eliminate_d,
         daeBuilder_eliminate_quad,
         daeBuilder_fun,
         daeBuilder_get_str__0,
         daeBuilder_get_str__1,
         daeBuilder_guess__0,
         daeBuilder_guess__1,
         daeBuilder_guess__2,
         daeBuilder_guess__3,
         daeBuilder_has_fun,
         daeBuilder_make_explicit,
         daeBuilder_make_semi_explicit,
         daeBuilder_max__0,
         daeBuilder_max__1,
         daeBuilder_max__2,
         daeBuilder_max__3,
         daeBuilder_min__0,
         daeBuilder_min__1,
         daeBuilder_min__2,
         daeBuilder_min__3,
         daeBuilder_nominal__0,
         daeBuilder_nominal__1,
         daeBuilder_operator__call,
         daeBuilder_parse_fmi,
         daeBuilder_sanity_check,
         daeBuilder_scale_equations,
         daeBuilder_scale_variables,
         daeBuilder_set_derivative_start__0,
         daeBuilder_set_derivative_start__1,
         daeBuilder_set_derivative_start__2,
         daeBuilder_set_derivative_start__3,
         daeBuilder_set_guess__0,
         daeBuilder_set_guess__1,
         daeBuilder_set_guess__2,
         daeBuilder_set_guess__3,
         daeBuilder_set_max__0,
         daeBuilder_set_max__1,
         daeBuilder_set_max__2,
         daeBuilder_set_max__3,
         daeBuilder_set_min__0,
         daeBuilder_set_min__1,
         daeBuilder_set_min__2,
         daeBuilder_set_min__3,
         daeBuilder_set_nominal__0,
         daeBuilder_set_nominal__1,
         daeBuilder_set_start__0,
         daeBuilder_set_start__1,
         daeBuilder_set_start__2,
         daeBuilder_set_start__3,
         daeBuilder_set_unit,
         daeBuilder_sort_alg,
         daeBuilder_sort_d,
         daeBuilder_sort_dae,
         daeBuilder_split_d,
         daeBuilder_split_dae,
         daeBuilder_start__0,
         daeBuilder_start__1,
         daeBuilder_start__2,
         daeBuilder_start__3,
         daeBuilder_type_name,
         daeBuilder_unit__0,
         daeBuilder_unit__1,
         daeBuilder_var,
         daeBuilder_variable__0,
         daeBuilder_variable__1,
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
foreign import ccall unsafe "casadi__DaeBuilder__CONSTRUCTOR" c_casadi__DaeBuilder__CONSTRUCTOR
  :: Ptr (Ptr StdString) -> IO (Ptr DaeBuilder')

casadi__DaeBuilder__CONSTRUCTOR
  :: IO DaeBuilder
casadi__DaeBuilder__CONSTRUCTOR  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__CONSTRUCTOR errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
daeBuilder :: IO DaeBuilder
daeBuilder = casadi__DaeBuilder__CONSTRUCTOR


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_alg" c_casadi__DaeBuilder__add_alg
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> Ptr MX' -> IO ()

casadi__DaeBuilder__add_alg
  :: DaeBuilder -> String -> MX -> IO ()
casadi__DaeBuilder__add_alg x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_alg errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_add_alg :: DaeBuilderClass a => a -> String -> MX -> IO ()
daeBuilder_add_alg x = casadi__DaeBuilder__add_alg (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_aux__0" c_casadi__DaeBuilder__add_aux__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO (Ptr MX')

casadi__DaeBuilder__add_aux__0
  :: DaeBuilder -> IO MX
casadi__DaeBuilder__add_aux__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_aux__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
daeBuilder_add_aux__0 :: DaeBuilderClass a => a -> IO MX
daeBuilder_add_aux__0 x = casadi__DaeBuilder__add_aux__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_aux__1" c_casadi__DaeBuilder__add_aux__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO (Ptr MX')

casadi__DaeBuilder__add_aux__1
  :: DaeBuilder -> String -> IO MX
casadi__DaeBuilder__add_aux__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_aux__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_add_aux__1 :: DaeBuilderClass a => a -> String -> IO MX
daeBuilder_add_aux__1 x = casadi__DaeBuilder__add_aux__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_aux__2" c_casadi__DaeBuilder__add_aux__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CLLong -> IO (Ptr MX')

casadi__DaeBuilder__add_aux__2
  :: DaeBuilder -> String -> Int -> IO MX
casadi__DaeBuilder__add_aux__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_aux__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_add_aux__2 :: DaeBuilderClass a => a -> String -> Int -> IO MX
daeBuilder_add_aux__2 x = casadi__DaeBuilder__add_aux__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_d" c_casadi__DaeBuilder__add_d
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> Ptr MX' -> IO (Ptr MX')

casadi__DaeBuilder__add_d
  :: DaeBuilder -> String -> MX -> IO MX
casadi__DaeBuilder__add_d x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_d errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_add_d :: DaeBuilderClass a => a -> String -> MX -> IO MX
daeBuilder_add_d x = casadi__DaeBuilder__add_d (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_dae" c_casadi__DaeBuilder__add_dae
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> Ptr MX' -> IO ()

casadi__DaeBuilder__add_dae
  :: DaeBuilder -> String -> MX -> IO ()
casadi__DaeBuilder__add_dae x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_dae errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_add_dae :: DaeBuilderClass a => a -> String -> MX -> IO ()
daeBuilder_add_dae x = casadi__DaeBuilder__add_dae (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_fun__0" c_casadi__DaeBuilder__add_fun__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> Ptr Importer' -> IO (Ptr Function')

casadi__DaeBuilder__add_fun__0
  :: DaeBuilder -> String -> Importer -> IO Function
casadi__DaeBuilder__add_fun__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_fun__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_add_fun__0 :: DaeBuilderClass a => a -> String -> Importer -> IO Function
daeBuilder_add_fun__0 x = casadi__DaeBuilder__add_fun__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_fun__1" c_casadi__DaeBuilder__add_fun__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> Ptr Importer' -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__DaeBuilder__add_fun__1
  :: DaeBuilder -> String -> Importer -> M.Map String GenericType -> IO Function
casadi__DaeBuilder__add_fun__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_fun__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
daeBuilder_add_fun__1 :: DaeBuilderClass a => a -> String -> Importer -> M.Map String GenericType -> IO Function
daeBuilder_add_fun__1 x = casadi__DaeBuilder__add_fun__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_fun__2" c_casadi__DaeBuilder__add_fun__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr Function' -> IO (Ptr Function')

casadi__DaeBuilder__add_fun__2
  :: DaeBuilder -> Function -> IO Function
casadi__DaeBuilder__add_fun__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_fun__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_add_fun__2 :: DaeBuilderClass a => a -> Function -> IO Function
daeBuilder_add_fun__2 x = casadi__DaeBuilder__add_fun__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_fun__3" c_casadi__DaeBuilder__add_fun__3
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> IO (Ptr Function')

casadi__DaeBuilder__add_fun__3
  :: DaeBuilder -> String -> Vector String -> Vector String -> IO Function
casadi__DaeBuilder__add_fun__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_fun__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
daeBuilder_add_fun__3 :: DaeBuilderClass a => a -> String -> Vector String -> Vector String -> IO Function
daeBuilder_add_fun__3 x = casadi__DaeBuilder__add_fun__3 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_fun__4" c_casadi__DaeBuilder__add_fun__4
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__DaeBuilder__add_fun__4
  :: DaeBuilder -> String -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
casadi__DaeBuilder__add_fun__4 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_fun__4 errStrPtrP x0' x1' x2' x3' x4'
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
daeBuilder_add_fun__4 :: DaeBuilderClass a => a -> String -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
daeBuilder_add_fun__4 x = casadi__DaeBuilder__add_fun__4 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_lc" c_casadi__DaeBuilder__add_lc
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> Ptr (StdVec (Ptr StdString)) -> IO (Ptr MX')

casadi__DaeBuilder__add_lc
  :: DaeBuilder -> String -> Vector String -> IO MX
casadi__DaeBuilder__add_lc x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_lc errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_add_lc :: DaeBuilderClass a => a -> String -> Vector String -> IO MX
daeBuilder_add_lc x = casadi__DaeBuilder__add_lc (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_ode" c_casadi__DaeBuilder__add_ode
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> Ptr MX' -> IO ()

casadi__DaeBuilder__add_ode
  :: DaeBuilder -> String -> MX -> IO ()
casadi__DaeBuilder__add_ode x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_ode errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_add_ode :: DaeBuilderClass a => a -> String -> MX -> IO ()
daeBuilder_add_ode x = casadi__DaeBuilder__add_ode (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_p__0" c_casadi__DaeBuilder__add_p__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO (Ptr MX')

casadi__DaeBuilder__add_p__0
  :: DaeBuilder -> IO MX
casadi__DaeBuilder__add_p__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_p__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
daeBuilder_add_p__0 :: DaeBuilderClass a => a -> IO MX
daeBuilder_add_p__0 x = casadi__DaeBuilder__add_p__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_p__1" c_casadi__DaeBuilder__add_p__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO (Ptr MX')

casadi__DaeBuilder__add_p__1
  :: DaeBuilder -> String -> IO MX
casadi__DaeBuilder__add_p__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_p__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_add_p__1 :: DaeBuilderClass a => a -> String -> IO MX
daeBuilder_add_p__1 x = casadi__DaeBuilder__add_p__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_p__2" c_casadi__DaeBuilder__add_p__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CLLong -> IO (Ptr MX')

casadi__DaeBuilder__add_p__2
  :: DaeBuilder -> String -> Int -> IO MX
casadi__DaeBuilder__add_p__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_p__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_add_p__2 :: DaeBuilderClass a => a -> String -> Int -> IO MX
daeBuilder_add_p__2 x = casadi__DaeBuilder__add_p__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_q__0" c_casadi__DaeBuilder__add_q__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO (Ptr MX')

casadi__DaeBuilder__add_q__0
  :: DaeBuilder -> IO MX
casadi__DaeBuilder__add_q__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_q__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
daeBuilder_add_q__0 :: DaeBuilderClass a => a -> IO MX
daeBuilder_add_q__0 x = casadi__DaeBuilder__add_q__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_q__1" c_casadi__DaeBuilder__add_q__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO (Ptr MX')

casadi__DaeBuilder__add_q__1
  :: DaeBuilder -> String -> IO MX
casadi__DaeBuilder__add_q__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_q__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_add_q__1 :: DaeBuilderClass a => a -> String -> IO MX
daeBuilder_add_q__1 x = casadi__DaeBuilder__add_q__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_q__2" c_casadi__DaeBuilder__add_q__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CLLong -> IO (Ptr MX')

casadi__DaeBuilder__add_q__2
  :: DaeBuilder -> String -> Int -> IO MX
casadi__DaeBuilder__add_q__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_q__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_add_q__2 :: DaeBuilderClass a => a -> String -> Int -> IO MX
daeBuilder_add_q__2 x = casadi__DaeBuilder__add_q__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_quad" c_casadi__DaeBuilder__add_quad
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> Ptr MX' -> IO ()

casadi__DaeBuilder__add_quad
  :: DaeBuilder -> String -> MX -> IO ()
casadi__DaeBuilder__add_quad x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_quad errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_add_quad :: DaeBuilderClass a => a -> String -> MX -> IO ()
daeBuilder_add_quad x = casadi__DaeBuilder__add_quad (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_s__0" c_casadi__DaeBuilder__add_s__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO (Ptr (StdPair (Ptr MX') (Ptr MX')))

casadi__DaeBuilder__add_s__0
  :: DaeBuilder -> IO (MX, MX)
casadi__DaeBuilder__add_s__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_s__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
daeBuilder_add_s__0 :: DaeBuilderClass a => a -> IO (MX, MX)
daeBuilder_add_s__0 x = casadi__DaeBuilder__add_s__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_s__1" c_casadi__DaeBuilder__add_s__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO (Ptr (StdPair (Ptr MX') (Ptr MX')))

casadi__DaeBuilder__add_s__1
  :: DaeBuilder -> String -> IO (MX, MX)
casadi__DaeBuilder__add_s__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_s__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_add_s__1 :: DaeBuilderClass a => a -> String -> IO (MX, MX)
daeBuilder_add_s__1 x = casadi__DaeBuilder__add_s__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_s__2" c_casadi__DaeBuilder__add_s__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CLLong -> IO (Ptr (StdPair (Ptr MX') (Ptr MX')))

casadi__DaeBuilder__add_s__2
  :: DaeBuilder -> String -> Int -> IO (MX, MX)
casadi__DaeBuilder__add_s__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_s__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_add_s__2 :: DaeBuilderClass a => a -> String -> Int -> IO (MX, MX)
daeBuilder_add_s__2 x = casadi__DaeBuilder__add_s__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_u__0" c_casadi__DaeBuilder__add_u__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO (Ptr MX')

casadi__DaeBuilder__add_u__0
  :: DaeBuilder -> IO MX
casadi__DaeBuilder__add_u__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_u__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
daeBuilder_add_u__0 :: DaeBuilderClass a => a -> IO MX
daeBuilder_add_u__0 x = casadi__DaeBuilder__add_u__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_u__1" c_casadi__DaeBuilder__add_u__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO (Ptr MX')

casadi__DaeBuilder__add_u__1
  :: DaeBuilder -> String -> IO MX
casadi__DaeBuilder__add_u__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_u__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_add_u__1 :: DaeBuilderClass a => a -> String -> IO MX
daeBuilder_add_u__1 x = casadi__DaeBuilder__add_u__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_u__2" c_casadi__DaeBuilder__add_u__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CLLong -> IO (Ptr MX')

casadi__DaeBuilder__add_u__2
  :: DaeBuilder -> String -> Int -> IO MX
casadi__DaeBuilder__add_u__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_u__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_add_u__2 :: DaeBuilderClass a => a -> String -> Int -> IO MX
daeBuilder_add_u__2 x = casadi__DaeBuilder__add_u__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_variable__0" c_casadi__DaeBuilder__add_variable__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> Ptr Sparsity' -> IO (Ptr MX')

casadi__DaeBuilder__add_variable__0
  :: DaeBuilder -> String -> Sparsity -> IO MX
casadi__DaeBuilder__add_variable__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_variable__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_add_variable__0 :: DaeBuilderClass a => a -> String -> Sparsity -> IO MX
daeBuilder_add_variable__0 x = casadi__DaeBuilder__add_variable__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_variable__1" c_casadi__DaeBuilder__add_variable__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO (Ptr MX')

casadi__DaeBuilder__add_variable__1
  :: DaeBuilder -> String -> IO MX
casadi__DaeBuilder__add_variable__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_variable__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_add_variable__1 :: DaeBuilderClass a => a -> String -> IO MX
daeBuilder_add_variable__1 x = casadi__DaeBuilder__add_variable__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_variable__2" c_casadi__DaeBuilder__add_variable__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CLLong -> IO (Ptr MX')

casadi__DaeBuilder__add_variable__2
  :: DaeBuilder -> String -> Int -> IO MX
casadi__DaeBuilder__add_variable__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_variable__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_add_variable__2 :: DaeBuilderClass a => a -> String -> Int -> IO MX
daeBuilder_add_variable__2 x = casadi__DaeBuilder__add_variable__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_variable__3" c_casadi__DaeBuilder__add_variable__3
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> Ptr Variable' -> IO ()

casadi__DaeBuilder__add_variable__3
  :: DaeBuilder -> String -> Variable -> IO ()
casadi__DaeBuilder__add_variable__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_variable__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_add_variable__3 :: DaeBuilderClass a => a -> String -> Variable -> IO ()
daeBuilder_add_variable__3 x = casadi__DaeBuilder__add_variable__3 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_x__0" c_casadi__DaeBuilder__add_x__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO (Ptr MX')

casadi__DaeBuilder__add_x__0
  :: DaeBuilder -> IO MX
casadi__DaeBuilder__add_x__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_x__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
daeBuilder_add_x__0 :: DaeBuilderClass a => a -> IO MX
daeBuilder_add_x__0 x = casadi__DaeBuilder__add_x__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_x__1" c_casadi__DaeBuilder__add_x__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO (Ptr MX')

casadi__DaeBuilder__add_x__1
  :: DaeBuilder -> String -> IO MX
casadi__DaeBuilder__add_x__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_x__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_add_x__1 :: DaeBuilderClass a => a -> String -> IO MX
daeBuilder_add_x__1 x = casadi__DaeBuilder__add_x__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_x__2" c_casadi__DaeBuilder__add_x__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CLLong -> IO (Ptr MX')

casadi__DaeBuilder__add_x__2
  :: DaeBuilder -> String -> Int -> IO MX
casadi__DaeBuilder__add_x__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_x__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_add_x__2 :: DaeBuilderClass a => a -> String -> Int -> IO MX
daeBuilder_add_x__2 x = casadi__DaeBuilder__add_x__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_y" c_casadi__DaeBuilder__add_y
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> Ptr MX' -> IO (Ptr MX')

casadi__DaeBuilder__add_y
  :: DaeBuilder -> String -> MX -> IO MX
casadi__DaeBuilder__add_y x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_y errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_add_y :: DaeBuilderClass a => a -> String -> MX -> IO MX
daeBuilder_add_y x = casadi__DaeBuilder__add_y (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_z__0" c_casadi__DaeBuilder__add_z__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO (Ptr MX')

casadi__DaeBuilder__add_z__0
  :: DaeBuilder -> IO MX
casadi__DaeBuilder__add_z__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_z__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
daeBuilder_add_z__0 :: DaeBuilderClass a => a -> IO MX
daeBuilder_add_z__0 x = casadi__DaeBuilder__add_z__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_z__1" c_casadi__DaeBuilder__add_z__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO (Ptr MX')

casadi__DaeBuilder__add_z__1
  :: DaeBuilder -> String -> IO MX
casadi__DaeBuilder__add_z__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_z__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_add_z__1 :: DaeBuilderClass a => a -> String -> IO MX
daeBuilder_add_z__1 x = casadi__DaeBuilder__add_z__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__add_z__2" c_casadi__DaeBuilder__add_z__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CLLong -> IO (Ptr MX')

casadi__DaeBuilder__add_z__2
  :: DaeBuilder -> String -> Int -> IO MX
casadi__DaeBuilder__add_z__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__add_z__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_add_z__2 :: DaeBuilderClass a => a -> String -> Int -> IO MX
daeBuilder_add_z__2 x = casadi__DaeBuilder__add_z__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__create" c_casadi__DaeBuilder__create
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> IO (Ptr Function')

casadi__DaeBuilder__create
  :: DaeBuilder -> String -> Vector String -> Vector String -> IO Function
casadi__DaeBuilder__create x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__create errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
daeBuilder_create :: DaeBuilderClass a => a -> String -> Vector String -> Vector String -> IO Function
daeBuilder_create x = casadi__DaeBuilder__create (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__der__0" c_casadi__DaeBuilder__der__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> IO (Ptr MX')

casadi__DaeBuilder__der__0
  :: DaeBuilder -> MX -> IO MX
casadi__DaeBuilder__der__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__der__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_der__0 :: DaeBuilderClass a => a -> MX -> IO MX
daeBuilder_der__0 x = casadi__DaeBuilder__der__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__der__1" c_casadi__DaeBuilder__der__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO (Ptr MX')

casadi__DaeBuilder__der__1
  :: DaeBuilder -> String -> IO MX
casadi__DaeBuilder__der__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__der__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_der__1 :: DaeBuilderClass a => a -> String -> IO MX
daeBuilder_der__1 x = casadi__DaeBuilder__der__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__derivative_start__0" c_casadi__DaeBuilder__derivative_start__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> IO (Ptr (StdVec CDouble))

casadi__DaeBuilder__derivative_start__0
  :: DaeBuilder -> MX -> IO (Vector Double)
casadi__DaeBuilder__derivative_start__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__derivative_start__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_derivative_start__0 :: DaeBuilderClass a => a -> MX -> IO (Vector Double)
daeBuilder_derivative_start__0 x = casadi__DaeBuilder__derivative_start__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__derivative_start__1" c_casadi__DaeBuilder__derivative_start__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> CInt -> IO (Ptr (StdVec CDouble))

casadi__DaeBuilder__derivative_start__1
  :: DaeBuilder -> MX -> Bool -> IO (Vector Double)
casadi__DaeBuilder__derivative_start__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__derivative_start__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_derivative_start__1 :: DaeBuilderClass a => a -> MX -> Bool -> IO (Vector Double)
daeBuilder_derivative_start__1 x = casadi__DaeBuilder__derivative_start__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__derivative_start__2" c_casadi__DaeBuilder__derivative_start__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO CDouble

casadi__DaeBuilder__derivative_start__2
  :: DaeBuilder -> String -> IO Double
casadi__DaeBuilder__derivative_start__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__derivative_start__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_derivative_start__2 :: DaeBuilderClass a => a -> String -> IO Double
daeBuilder_derivative_start__2 x = casadi__DaeBuilder__derivative_start__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__derivative_start__3" c_casadi__DaeBuilder__derivative_start__3
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CInt -> IO CDouble

casadi__DaeBuilder__derivative_start__3
  :: DaeBuilder -> String -> Bool -> IO Double
casadi__DaeBuilder__derivative_start__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__derivative_start__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_derivative_start__3 :: DaeBuilderClass a => a -> String -> Bool -> IO Double
daeBuilder_derivative_start__3 x = casadi__DaeBuilder__derivative_start__3 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__eliminate_alg" c_casadi__DaeBuilder__eliminate_alg
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO ()

casadi__DaeBuilder__eliminate_alg
  :: DaeBuilder -> IO ()
casadi__DaeBuilder__eliminate_alg x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__eliminate_alg errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
daeBuilder_eliminate_alg :: DaeBuilderClass a => a -> IO ()
daeBuilder_eliminate_alg x = casadi__DaeBuilder__eliminate_alg (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__eliminate_d" c_casadi__DaeBuilder__eliminate_d
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO ()

casadi__DaeBuilder__eliminate_d
  :: DaeBuilder -> IO ()
casadi__DaeBuilder__eliminate_d x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__eliminate_d errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
daeBuilder_eliminate_d :: DaeBuilderClass a => a -> IO ()
daeBuilder_eliminate_d x = casadi__DaeBuilder__eliminate_d (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__eliminate_quad" c_casadi__DaeBuilder__eliminate_quad
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO ()

casadi__DaeBuilder__eliminate_quad
  :: DaeBuilder -> IO ()
casadi__DaeBuilder__eliminate_quad x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__eliminate_quad errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
daeBuilder_eliminate_quad :: DaeBuilderClass a => a -> IO ()
daeBuilder_eliminate_quad x = casadi__DaeBuilder__eliminate_quad (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__fun" c_casadi__DaeBuilder__fun
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO (Ptr Function')

casadi__DaeBuilder__fun
  :: DaeBuilder -> String -> IO Function
casadi__DaeBuilder__fun x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__fun errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_fun :: DaeBuilderClass a => a -> String -> IO Function
daeBuilder_fun x = casadi__DaeBuilder__fun (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__get_str__0" c_casadi__DaeBuilder__get_str__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO (Ptr StdString)

casadi__DaeBuilder__get_str__0
  :: DaeBuilder -> IO String
casadi__DaeBuilder__get_str__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__get_str__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
daeBuilder_get_str__0 :: DaeBuilderClass a => a -> IO String
daeBuilder_get_str__0 x = casadi__DaeBuilder__get_str__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__get_str__1" c_casadi__DaeBuilder__get_str__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> CInt -> IO (Ptr StdString)

casadi__DaeBuilder__get_str__1
  :: DaeBuilder -> Bool -> IO String
casadi__DaeBuilder__get_str__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__get_str__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_get_str__1 :: DaeBuilderClass a => a -> Bool -> IO String
daeBuilder_get_str__1 x = casadi__DaeBuilder__get_str__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__guess__0" c_casadi__DaeBuilder__guess__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> IO (Ptr (StdVec CDouble))

casadi__DaeBuilder__guess__0
  :: DaeBuilder -> MX -> IO (Vector Double)
casadi__DaeBuilder__guess__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__guess__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_guess__0 :: DaeBuilderClass a => a -> MX -> IO (Vector Double)
daeBuilder_guess__0 x = casadi__DaeBuilder__guess__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__guess__1" c_casadi__DaeBuilder__guess__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> CInt -> IO (Ptr (StdVec CDouble))

casadi__DaeBuilder__guess__1
  :: DaeBuilder -> MX -> Bool -> IO (Vector Double)
casadi__DaeBuilder__guess__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__guess__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_guess__1 :: DaeBuilderClass a => a -> MX -> Bool -> IO (Vector Double)
daeBuilder_guess__1 x = casadi__DaeBuilder__guess__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__guess__2" c_casadi__DaeBuilder__guess__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO CDouble

casadi__DaeBuilder__guess__2
  :: DaeBuilder -> String -> IO Double
casadi__DaeBuilder__guess__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__guess__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_guess__2 :: DaeBuilderClass a => a -> String -> IO Double
daeBuilder_guess__2 x = casadi__DaeBuilder__guess__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__guess__3" c_casadi__DaeBuilder__guess__3
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CInt -> IO CDouble

casadi__DaeBuilder__guess__3
  :: DaeBuilder -> String -> Bool -> IO Double
casadi__DaeBuilder__guess__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__guess__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_guess__3 :: DaeBuilderClass a => a -> String -> Bool -> IO Double
daeBuilder_guess__3 x = casadi__DaeBuilder__guess__3 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__has_fun" c_casadi__DaeBuilder__has_fun
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO CInt

casadi__DaeBuilder__has_fun
  :: DaeBuilder -> String -> IO Bool
casadi__DaeBuilder__has_fun x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__has_fun errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_has_fun :: DaeBuilderClass a => a -> String -> IO Bool
daeBuilder_has_fun x = casadi__DaeBuilder__has_fun (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__make_explicit" c_casadi__DaeBuilder__make_explicit
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO ()

casadi__DaeBuilder__make_explicit
  :: DaeBuilder -> IO ()
casadi__DaeBuilder__make_explicit x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__make_explicit errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
daeBuilder_make_explicit :: DaeBuilderClass a => a -> IO ()
daeBuilder_make_explicit x = casadi__DaeBuilder__make_explicit (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__make_semi_explicit" c_casadi__DaeBuilder__make_semi_explicit
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO ()

casadi__DaeBuilder__make_semi_explicit
  :: DaeBuilder -> IO ()
casadi__DaeBuilder__make_semi_explicit x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__make_semi_explicit errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
daeBuilder_make_semi_explicit :: DaeBuilderClass a => a -> IO ()
daeBuilder_make_semi_explicit x = casadi__DaeBuilder__make_semi_explicit (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__max__0" c_casadi__DaeBuilder__max__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> IO (Ptr (StdVec CDouble))

casadi__DaeBuilder__max__0
  :: DaeBuilder -> MX -> IO (Vector Double)
casadi__DaeBuilder__max__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__max__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_max__0 :: DaeBuilderClass a => a -> MX -> IO (Vector Double)
daeBuilder_max__0 x = casadi__DaeBuilder__max__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__max__1" c_casadi__DaeBuilder__max__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> CInt -> IO (Ptr (StdVec CDouble))

casadi__DaeBuilder__max__1
  :: DaeBuilder -> MX -> Bool -> IO (Vector Double)
casadi__DaeBuilder__max__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__max__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_max__1 :: DaeBuilderClass a => a -> MX -> Bool -> IO (Vector Double)
daeBuilder_max__1 x = casadi__DaeBuilder__max__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__max__2" c_casadi__DaeBuilder__max__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO CDouble

casadi__DaeBuilder__max__2
  :: DaeBuilder -> String -> IO Double
casadi__DaeBuilder__max__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__max__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_max__2 :: DaeBuilderClass a => a -> String -> IO Double
daeBuilder_max__2 x = casadi__DaeBuilder__max__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__max__3" c_casadi__DaeBuilder__max__3
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CInt -> IO CDouble

casadi__DaeBuilder__max__3
  :: DaeBuilder -> String -> Bool -> IO Double
casadi__DaeBuilder__max__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__max__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_max__3 :: DaeBuilderClass a => a -> String -> Bool -> IO Double
daeBuilder_max__3 x = casadi__DaeBuilder__max__3 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__min__0" c_casadi__DaeBuilder__min__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> IO (Ptr (StdVec CDouble))

casadi__DaeBuilder__min__0
  :: DaeBuilder -> MX -> IO (Vector Double)
casadi__DaeBuilder__min__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__min__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_min__0 :: DaeBuilderClass a => a -> MX -> IO (Vector Double)
daeBuilder_min__0 x = casadi__DaeBuilder__min__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__min__1" c_casadi__DaeBuilder__min__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> CInt -> IO (Ptr (StdVec CDouble))

casadi__DaeBuilder__min__1
  :: DaeBuilder -> MX -> Bool -> IO (Vector Double)
casadi__DaeBuilder__min__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__min__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_min__1 :: DaeBuilderClass a => a -> MX -> Bool -> IO (Vector Double)
daeBuilder_min__1 x = casadi__DaeBuilder__min__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__min__2" c_casadi__DaeBuilder__min__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO CDouble

casadi__DaeBuilder__min__2
  :: DaeBuilder -> String -> IO Double
casadi__DaeBuilder__min__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__min__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_min__2 :: DaeBuilderClass a => a -> String -> IO Double
daeBuilder_min__2 x = casadi__DaeBuilder__min__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__min__3" c_casadi__DaeBuilder__min__3
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CInt -> IO CDouble

casadi__DaeBuilder__min__3
  :: DaeBuilder -> String -> Bool -> IO Double
casadi__DaeBuilder__min__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__min__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_min__3 :: DaeBuilderClass a => a -> String -> Bool -> IO Double
daeBuilder_min__3 x = casadi__DaeBuilder__min__3 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__nominal__0" c_casadi__DaeBuilder__nominal__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> IO (Ptr (StdVec CDouble))

casadi__DaeBuilder__nominal__0
  :: DaeBuilder -> MX -> IO (Vector Double)
casadi__DaeBuilder__nominal__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__nominal__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_nominal__0 :: DaeBuilderClass a => a -> MX -> IO (Vector Double)
daeBuilder_nominal__0 x = casadi__DaeBuilder__nominal__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__nominal__1" c_casadi__DaeBuilder__nominal__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO CDouble

casadi__DaeBuilder__nominal__1
  :: DaeBuilder -> String -> IO Double
casadi__DaeBuilder__nominal__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__nominal__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_nominal__1 :: DaeBuilderClass a => a -> String -> IO Double
daeBuilder_nominal__1 x = casadi__DaeBuilder__nominal__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__operator__call" c_casadi__DaeBuilder__operator__call
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO (Ptr MX')

casadi__DaeBuilder__operator__call
  :: DaeBuilder -> String -> IO MX
casadi__DaeBuilder__operator__call x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__operator__call errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_operator__call :: DaeBuilderClass a => a -> String -> IO MX
daeBuilder_operator__call x = casadi__DaeBuilder__operator__call (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__parse_fmi" c_casadi__DaeBuilder__parse_fmi
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO ()

casadi__DaeBuilder__parse_fmi
  :: DaeBuilder -> String -> IO ()
casadi__DaeBuilder__parse_fmi x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__parse_fmi errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
daeBuilder_parse_fmi :: DaeBuilderClass a => a -> String -> IO ()
daeBuilder_parse_fmi x = casadi__DaeBuilder__parse_fmi (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__sanity_check" c_casadi__DaeBuilder__sanity_check
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO ()

casadi__DaeBuilder__sanity_check
  :: DaeBuilder -> IO ()
casadi__DaeBuilder__sanity_check x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__sanity_check errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
daeBuilder_sanity_check :: DaeBuilderClass a => a -> IO ()
daeBuilder_sanity_check x = casadi__DaeBuilder__sanity_check (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__scale_equations" c_casadi__DaeBuilder__scale_equations
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO ()

casadi__DaeBuilder__scale_equations
  :: DaeBuilder -> IO ()
casadi__DaeBuilder__scale_equations x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__scale_equations errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
daeBuilder_scale_equations :: DaeBuilderClass a => a -> IO ()
daeBuilder_scale_equations x = casadi__DaeBuilder__scale_equations (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__scale_variables" c_casadi__DaeBuilder__scale_variables
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO ()

casadi__DaeBuilder__scale_variables
  :: DaeBuilder -> IO ()
casadi__DaeBuilder__scale_variables x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__scale_variables errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
daeBuilder_scale_variables :: DaeBuilderClass a => a -> IO ()
daeBuilder_scale_variables x = casadi__DaeBuilder__scale_variables (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_derivative_start__0" c_casadi__DaeBuilder__set_derivative_start__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> Ptr (StdVec CDouble) -> IO ()

casadi__DaeBuilder__set_derivative_start__0
  :: DaeBuilder -> MX -> Vector Double -> IO ()
casadi__DaeBuilder__set_derivative_start__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_derivative_start__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_set_derivative_start__0 :: DaeBuilderClass a => a -> MX -> Vector Double -> IO ()
daeBuilder_set_derivative_start__0 x = casadi__DaeBuilder__set_derivative_start__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_derivative_start__1" c_casadi__DaeBuilder__set_derivative_start__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> Ptr (StdVec CDouble) -> CInt -> IO ()

casadi__DaeBuilder__set_derivative_start__1
  :: DaeBuilder -> MX -> Vector Double -> Bool -> IO ()
casadi__DaeBuilder__set_derivative_start__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_derivative_start__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
daeBuilder_set_derivative_start__1 :: DaeBuilderClass a => a -> MX -> Vector Double -> Bool -> IO ()
daeBuilder_set_derivative_start__1 x = casadi__DaeBuilder__set_derivative_start__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_derivative_start__2" c_casadi__DaeBuilder__set_derivative_start__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CDouble -> IO ()

casadi__DaeBuilder__set_derivative_start__2
  :: DaeBuilder -> String -> Double -> IO ()
casadi__DaeBuilder__set_derivative_start__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_derivative_start__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_set_derivative_start__2 :: DaeBuilderClass a => a -> String -> Double -> IO ()
daeBuilder_set_derivative_start__2 x = casadi__DaeBuilder__set_derivative_start__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_derivative_start__3" c_casadi__DaeBuilder__set_derivative_start__3
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CDouble -> CInt -> IO ()

casadi__DaeBuilder__set_derivative_start__3
  :: DaeBuilder -> String -> Double -> Bool -> IO ()
casadi__DaeBuilder__set_derivative_start__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_derivative_start__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
daeBuilder_set_derivative_start__3 :: DaeBuilderClass a => a -> String -> Double -> Bool -> IO ()
daeBuilder_set_derivative_start__3 x = casadi__DaeBuilder__set_derivative_start__3 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_guess__0" c_casadi__DaeBuilder__set_guess__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> Ptr (StdVec CDouble) -> IO ()

casadi__DaeBuilder__set_guess__0
  :: DaeBuilder -> MX -> Vector Double -> IO ()
casadi__DaeBuilder__set_guess__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_guess__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_set_guess__0 :: DaeBuilderClass a => a -> MX -> Vector Double -> IO ()
daeBuilder_set_guess__0 x = casadi__DaeBuilder__set_guess__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_guess__1" c_casadi__DaeBuilder__set_guess__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> Ptr (StdVec CDouble) -> CInt -> IO ()

casadi__DaeBuilder__set_guess__1
  :: DaeBuilder -> MX -> Vector Double -> Bool -> IO ()
casadi__DaeBuilder__set_guess__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_guess__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
daeBuilder_set_guess__1 :: DaeBuilderClass a => a -> MX -> Vector Double -> Bool -> IO ()
daeBuilder_set_guess__1 x = casadi__DaeBuilder__set_guess__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_guess__2" c_casadi__DaeBuilder__set_guess__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CDouble -> IO ()

casadi__DaeBuilder__set_guess__2
  :: DaeBuilder -> String -> Double -> IO ()
casadi__DaeBuilder__set_guess__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_guess__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_set_guess__2 :: DaeBuilderClass a => a -> String -> Double -> IO ()
daeBuilder_set_guess__2 x = casadi__DaeBuilder__set_guess__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_guess__3" c_casadi__DaeBuilder__set_guess__3
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CDouble -> CInt -> IO ()

casadi__DaeBuilder__set_guess__3
  :: DaeBuilder -> String -> Double -> Bool -> IO ()
casadi__DaeBuilder__set_guess__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_guess__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
daeBuilder_set_guess__3 :: DaeBuilderClass a => a -> String -> Double -> Bool -> IO ()
daeBuilder_set_guess__3 x = casadi__DaeBuilder__set_guess__3 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_max__0" c_casadi__DaeBuilder__set_max__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> Ptr (StdVec CDouble) -> IO ()

casadi__DaeBuilder__set_max__0
  :: DaeBuilder -> MX -> Vector Double -> IO ()
casadi__DaeBuilder__set_max__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_max__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_set_max__0 :: DaeBuilderClass a => a -> MX -> Vector Double -> IO ()
daeBuilder_set_max__0 x = casadi__DaeBuilder__set_max__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_max__1" c_casadi__DaeBuilder__set_max__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> Ptr (StdVec CDouble) -> CInt -> IO ()

casadi__DaeBuilder__set_max__1
  :: DaeBuilder -> MX -> Vector Double -> Bool -> IO ()
casadi__DaeBuilder__set_max__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_max__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
daeBuilder_set_max__1 :: DaeBuilderClass a => a -> MX -> Vector Double -> Bool -> IO ()
daeBuilder_set_max__1 x = casadi__DaeBuilder__set_max__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_max__2" c_casadi__DaeBuilder__set_max__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CDouble -> IO ()

casadi__DaeBuilder__set_max__2
  :: DaeBuilder -> String -> Double -> IO ()
casadi__DaeBuilder__set_max__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_max__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_set_max__2 :: DaeBuilderClass a => a -> String -> Double -> IO ()
daeBuilder_set_max__2 x = casadi__DaeBuilder__set_max__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_max__3" c_casadi__DaeBuilder__set_max__3
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CDouble -> CInt -> IO ()

casadi__DaeBuilder__set_max__3
  :: DaeBuilder -> String -> Double -> Bool -> IO ()
casadi__DaeBuilder__set_max__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_max__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
daeBuilder_set_max__3 :: DaeBuilderClass a => a -> String -> Double -> Bool -> IO ()
daeBuilder_set_max__3 x = casadi__DaeBuilder__set_max__3 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_min__0" c_casadi__DaeBuilder__set_min__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> Ptr (StdVec CDouble) -> IO ()

casadi__DaeBuilder__set_min__0
  :: DaeBuilder -> MX -> Vector Double -> IO ()
casadi__DaeBuilder__set_min__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_min__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_set_min__0 :: DaeBuilderClass a => a -> MX -> Vector Double -> IO ()
daeBuilder_set_min__0 x = casadi__DaeBuilder__set_min__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_min__1" c_casadi__DaeBuilder__set_min__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> Ptr (StdVec CDouble) -> CInt -> IO ()

casadi__DaeBuilder__set_min__1
  :: DaeBuilder -> MX -> Vector Double -> Bool -> IO ()
casadi__DaeBuilder__set_min__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_min__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
daeBuilder_set_min__1 :: DaeBuilderClass a => a -> MX -> Vector Double -> Bool -> IO ()
daeBuilder_set_min__1 x = casadi__DaeBuilder__set_min__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_min__2" c_casadi__DaeBuilder__set_min__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CDouble -> IO ()

casadi__DaeBuilder__set_min__2
  :: DaeBuilder -> String -> Double -> IO ()
casadi__DaeBuilder__set_min__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_min__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_set_min__2 :: DaeBuilderClass a => a -> String -> Double -> IO ()
daeBuilder_set_min__2 x = casadi__DaeBuilder__set_min__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_min__3" c_casadi__DaeBuilder__set_min__3
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CDouble -> CInt -> IO ()

casadi__DaeBuilder__set_min__3
  :: DaeBuilder -> String -> Double -> Bool -> IO ()
casadi__DaeBuilder__set_min__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_min__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
daeBuilder_set_min__3 :: DaeBuilderClass a => a -> String -> Double -> Bool -> IO ()
daeBuilder_set_min__3 x = casadi__DaeBuilder__set_min__3 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_nominal__0" c_casadi__DaeBuilder__set_nominal__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> Ptr (StdVec CDouble) -> IO ()

casadi__DaeBuilder__set_nominal__0
  :: DaeBuilder -> MX -> Vector Double -> IO ()
casadi__DaeBuilder__set_nominal__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_nominal__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_set_nominal__0 :: DaeBuilderClass a => a -> MX -> Vector Double -> IO ()
daeBuilder_set_nominal__0 x = casadi__DaeBuilder__set_nominal__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_nominal__1" c_casadi__DaeBuilder__set_nominal__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CDouble -> IO ()

casadi__DaeBuilder__set_nominal__1
  :: DaeBuilder -> String -> Double -> IO ()
casadi__DaeBuilder__set_nominal__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_nominal__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_set_nominal__1 :: DaeBuilderClass a => a -> String -> Double -> IO ()
daeBuilder_set_nominal__1 x = casadi__DaeBuilder__set_nominal__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_start__0" c_casadi__DaeBuilder__set_start__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> Ptr (StdVec CDouble) -> IO ()

casadi__DaeBuilder__set_start__0
  :: DaeBuilder -> MX -> Vector Double -> IO ()
casadi__DaeBuilder__set_start__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_start__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_set_start__0 :: DaeBuilderClass a => a -> MX -> Vector Double -> IO ()
daeBuilder_set_start__0 x = casadi__DaeBuilder__set_start__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_start__1" c_casadi__DaeBuilder__set_start__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> Ptr (StdVec CDouble) -> CInt -> IO ()

casadi__DaeBuilder__set_start__1
  :: DaeBuilder -> MX -> Vector Double -> Bool -> IO ()
casadi__DaeBuilder__set_start__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_start__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
daeBuilder_set_start__1 :: DaeBuilderClass a => a -> MX -> Vector Double -> Bool -> IO ()
daeBuilder_set_start__1 x = casadi__DaeBuilder__set_start__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_start__2" c_casadi__DaeBuilder__set_start__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CDouble -> IO ()

casadi__DaeBuilder__set_start__2
  :: DaeBuilder -> String -> Double -> IO ()
casadi__DaeBuilder__set_start__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_start__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_set_start__2 :: DaeBuilderClass a => a -> String -> Double -> IO ()
daeBuilder_set_start__2 x = casadi__DaeBuilder__set_start__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_start__3" c_casadi__DaeBuilder__set_start__3
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CDouble -> CInt -> IO ()

casadi__DaeBuilder__set_start__3
  :: DaeBuilder -> String -> Double -> Bool -> IO ()
casadi__DaeBuilder__set_start__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_start__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
daeBuilder_set_start__3 :: DaeBuilderClass a => a -> String -> Double -> Bool -> IO ()
daeBuilder_set_start__3 x = casadi__DaeBuilder__set_start__3 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__set_unit" c_casadi__DaeBuilder__set_unit
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> Ptr StdString -> IO ()

casadi__DaeBuilder__set_unit
  :: DaeBuilder -> String -> String -> IO ()
casadi__DaeBuilder__set_unit x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__set_unit errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
daeBuilder_set_unit :: DaeBuilderClass a => a -> String -> String -> IO ()
daeBuilder_set_unit x = casadi__DaeBuilder__set_unit (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__sort_alg" c_casadi__DaeBuilder__sort_alg
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO ()

casadi__DaeBuilder__sort_alg
  :: DaeBuilder -> IO ()
casadi__DaeBuilder__sort_alg x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__sort_alg errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
daeBuilder_sort_alg :: DaeBuilderClass a => a -> IO ()
daeBuilder_sort_alg x = casadi__DaeBuilder__sort_alg (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__sort_d" c_casadi__DaeBuilder__sort_d
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO ()

casadi__DaeBuilder__sort_d
  :: DaeBuilder -> IO ()
casadi__DaeBuilder__sort_d x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__sort_d errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
daeBuilder_sort_d :: DaeBuilderClass a => a -> IO ()
daeBuilder_sort_d x = casadi__DaeBuilder__sort_d (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__sort_dae" c_casadi__DaeBuilder__sort_dae
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO ()

casadi__DaeBuilder__sort_dae
  :: DaeBuilder -> IO ()
casadi__DaeBuilder__sort_dae x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__sort_dae errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
daeBuilder_sort_dae :: DaeBuilderClass a => a -> IO ()
daeBuilder_sort_dae x = casadi__DaeBuilder__sort_dae (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__split_d" c_casadi__DaeBuilder__split_d
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO ()

casadi__DaeBuilder__split_d
  :: DaeBuilder -> IO ()
casadi__DaeBuilder__split_d x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__split_d errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
daeBuilder_split_d :: DaeBuilderClass a => a -> IO ()
daeBuilder_split_d x = casadi__DaeBuilder__split_d (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__split_dae" c_casadi__DaeBuilder__split_dae
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO ()

casadi__DaeBuilder__split_dae
  :: DaeBuilder -> IO ()
casadi__DaeBuilder__split_dae x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__split_dae errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
daeBuilder_split_dae :: DaeBuilderClass a => a -> IO ()
daeBuilder_split_dae x = casadi__DaeBuilder__split_dae (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__start__0" c_casadi__DaeBuilder__start__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> IO (Ptr (StdVec CDouble))

casadi__DaeBuilder__start__0
  :: DaeBuilder -> MX -> IO (Vector Double)
casadi__DaeBuilder__start__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__start__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_start__0 :: DaeBuilderClass a => a -> MX -> IO (Vector Double)
daeBuilder_start__0 x = casadi__DaeBuilder__start__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__start__1" c_casadi__DaeBuilder__start__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> CInt -> IO (Ptr (StdVec CDouble))

casadi__DaeBuilder__start__1
  :: DaeBuilder -> MX -> Bool -> IO (Vector Double)
casadi__DaeBuilder__start__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__start__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_start__1 :: DaeBuilderClass a => a -> MX -> Bool -> IO (Vector Double)
daeBuilder_start__1 x = casadi__DaeBuilder__start__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__start__2" c_casadi__DaeBuilder__start__2
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO CDouble

casadi__DaeBuilder__start__2
  :: DaeBuilder -> String -> IO Double
casadi__DaeBuilder__start__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__start__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_start__2 :: DaeBuilderClass a => a -> String -> IO Double
daeBuilder_start__2 x = casadi__DaeBuilder__start__2 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__start__3" c_casadi__DaeBuilder__start__3
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> CInt -> IO CDouble

casadi__DaeBuilder__start__3
  :: DaeBuilder -> String -> Bool -> IO Double
casadi__DaeBuilder__start__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__start__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
daeBuilder_start__3 :: DaeBuilderClass a => a -> String -> Bool -> IO Double
daeBuilder_start__3 x = casadi__DaeBuilder__start__3 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__type_name" c_casadi__DaeBuilder__type_name
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> IO (Ptr StdString)

casadi__DaeBuilder__type_name
  :: DaeBuilder -> IO String
casadi__DaeBuilder__type_name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__type_name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
daeBuilder_type_name :: DaeBuilderClass a => a -> IO String
daeBuilder_type_name x = casadi__DaeBuilder__type_name (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__unit__0" c_casadi__DaeBuilder__unit__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr MX' -> IO (Ptr StdString)

casadi__DaeBuilder__unit__0
  :: DaeBuilder -> MX -> IO String
casadi__DaeBuilder__unit__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__unit__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_unit__0 :: DaeBuilderClass a => a -> MX -> IO String
daeBuilder_unit__0 x = casadi__DaeBuilder__unit__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__unit__1" c_casadi__DaeBuilder__unit__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO (Ptr StdString)

casadi__DaeBuilder__unit__1
  :: DaeBuilder -> String -> IO String
casadi__DaeBuilder__unit__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__unit__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_unit__1 :: DaeBuilderClass a => a -> String -> IO String
daeBuilder_unit__1 x = casadi__DaeBuilder__unit__1 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__var" c_casadi__DaeBuilder__var
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO (Ptr MX')

casadi__DaeBuilder__var
  :: DaeBuilder -> String -> IO MX
casadi__DaeBuilder__var x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__var errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_var :: DaeBuilderClass a => a -> String -> IO MX
daeBuilder_var x = casadi__DaeBuilder__var (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__variable__0" c_casadi__DaeBuilder__variable__0
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO (Ptr Variable')

casadi__DaeBuilder__variable__0
  :: DaeBuilder -> String -> IO Variable
casadi__DaeBuilder__variable__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__variable__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_variable__0 :: DaeBuilderClass a => a -> String -> IO Variable
daeBuilder_variable__0 x = casadi__DaeBuilder__variable__0 (castDaeBuilder x)


-- direct wrapper
foreign import ccall unsafe "casadi__DaeBuilder__variable__1" c_casadi__DaeBuilder__variable__1
  :: Ptr (Ptr StdString) -> Ptr DaeBuilder' -> Ptr StdString -> IO (Ptr Variable')

casadi__DaeBuilder__variable__1
  :: DaeBuilder -> String -> IO Variable
casadi__DaeBuilder__variable__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DaeBuilder__variable__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
daeBuilder_variable__1 :: DaeBuilderClass a => a -> String -> IO Variable
daeBuilder_variable__1 x = casadi__DaeBuilder__variable__1 (castDaeBuilder x)

