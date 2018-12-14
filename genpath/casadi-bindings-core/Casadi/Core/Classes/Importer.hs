{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.Importer
       (
         Importer,
         ImporterClass(..),
         importer__0,
         importer__1,
         importer__2,
         importer_body,
         importer_doc,
         importer_get_meta__0,
         importer_get_meta__1,
         importer_has_function,
         importer_has_meta__0,
         importer_has_meta__1,
         importer_has_plugin,
         importer_inlined,
         importer_load_plugin,
         importer_plugin_name,
         importer_type_name,
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
foreign import ccall unsafe "casadi__Importer__CONSTRUCTOR__0" c_casadi__Importer__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> IO (Ptr Importer')

casadi__Importer__CONSTRUCTOR__0
  :: String -> String -> IO Importer
casadi__Importer__CONSTRUCTOR__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Importer__CONSTRUCTOR__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
importer__0 :: String -> String -> IO Importer
importer__0 = casadi__Importer__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__Importer__CONSTRUCTOR__1" c_casadi__Importer__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Importer')

casadi__Importer__CONSTRUCTOR__1
  :: String -> String -> M.Map String GenericType -> IO Importer
casadi__Importer__CONSTRUCTOR__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Importer__CONSTRUCTOR__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
importer__1 :: String -> String -> M.Map String GenericType -> IO Importer
importer__1 = casadi__Importer__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__Importer__CONSTRUCTOR__2" c_casadi__Importer__CONSTRUCTOR__2
  :: Ptr (Ptr StdString) -> IO (Ptr Importer')

casadi__Importer__CONSTRUCTOR__2
  :: IO Importer
casadi__Importer__CONSTRUCTOR__2  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Importer__CONSTRUCTOR__2 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
importer__2 :: IO Importer
importer__2 = casadi__Importer__CONSTRUCTOR__2


-- direct wrapper
foreign import ccall unsafe "casadi__Importer__body" c_casadi__Importer__body
  :: Ptr (Ptr StdString) -> Ptr Importer' -> Ptr StdString -> IO (Ptr StdString)

casadi__Importer__body
  :: Importer -> String -> IO String
casadi__Importer__body x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Importer__body errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
importer_body :: ImporterClass a => a -> String -> IO String
importer_body x = casadi__Importer__body (castImporter x)


-- direct wrapper
foreign import ccall unsafe "casadi__Importer__doc" c_casadi__Importer__doc
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr StdString)

casadi__Importer__doc
  :: String -> IO String
casadi__Importer__doc x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Importer__doc errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
importer_doc :: String -> IO String
importer_doc = casadi__Importer__doc


-- direct wrapper
foreign import ccall unsafe "casadi__Importer__get_meta__0" c_casadi__Importer__get_meta__0
  :: Ptr (Ptr StdString) -> Ptr Importer' -> Ptr StdString -> IO (Ptr StdString)

casadi__Importer__get_meta__0
  :: Importer -> String -> IO String
casadi__Importer__get_meta__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Importer__get_meta__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
importer_get_meta__0 :: ImporterClass a => a -> String -> IO String
importer_get_meta__0 x = casadi__Importer__get_meta__0 (castImporter x)


-- direct wrapper
foreign import ccall unsafe "casadi__Importer__get_meta__1" c_casadi__Importer__get_meta__1
  :: Ptr (Ptr StdString) -> Ptr Importer' -> Ptr StdString -> CLLong -> IO (Ptr StdString)

casadi__Importer__get_meta__1
  :: Importer -> String -> Int -> IO String
casadi__Importer__get_meta__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Importer__get_meta__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
importer_get_meta__1 :: ImporterClass a => a -> String -> Int -> IO String
importer_get_meta__1 x = casadi__Importer__get_meta__1 (castImporter x)


-- direct wrapper
foreign import ccall unsafe "casadi__Importer__has_function" c_casadi__Importer__has_function
  :: Ptr (Ptr StdString) -> Ptr Importer' -> Ptr StdString -> IO CInt

casadi__Importer__has_function
  :: Importer -> String -> IO Bool
casadi__Importer__has_function x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Importer__has_function errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
importer_has_function :: ImporterClass a => a -> String -> IO Bool
importer_has_function x = casadi__Importer__has_function (castImporter x)


-- direct wrapper
foreign import ccall unsafe "casadi__Importer__has_meta__0" c_casadi__Importer__has_meta__0
  :: Ptr (Ptr StdString) -> Ptr Importer' -> Ptr StdString -> IO CInt

casadi__Importer__has_meta__0
  :: Importer -> String -> IO Bool
casadi__Importer__has_meta__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Importer__has_meta__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
importer_has_meta__0 :: ImporterClass a => a -> String -> IO Bool
importer_has_meta__0 x = casadi__Importer__has_meta__0 (castImporter x)


-- direct wrapper
foreign import ccall unsafe "casadi__Importer__has_meta__1" c_casadi__Importer__has_meta__1
  :: Ptr (Ptr StdString) -> Ptr Importer' -> Ptr StdString -> CLLong -> IO CInt

casadi__Importer__has_meta__1
  :: Importer -> String -> Int -> IO Bool
casadi__Importer__has_meta__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Importer__has_meta__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
importer_has_meta__1 :: ImporterClass a => a -> String -> Int -> IO Bool
importer_has_meta__1 x = casadi__Importer__has_meta__1 (castImporter x)


-- direct wrapper
foreign import ccall unsafe "casadi__Importer__has_plugin" c_casadi__Importer__has_plugin
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO CInt

casadi__Importer__has_plugin
  :: String -> IO Bool
casadi__Importer__has_plugin x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Importer__has_plugin errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
importer_has_plugin :: String -> IO Bool
importer_has_plugin = casadi__Importer__has_plugin


-- direct wrapper
foreign import ccall unsafe "casadi__Importer__inlined" c_casadi__Importer__inlined
  :: Ptr (Ptr StdString) -> Ptr Importer' -> Ptr StdString -> IO CInt

casadi__Importer__inlined
  :: Importer -> String -> IO Bool
casadi__Importer__inlined x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Importer__inlined errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
importer_inlined :: ImporterClass a => a -> String -> IO Bool
importer_inlined x = casadi__Importer__inlined (castImporter x)


-- direct wrapper
foreign import ccall unsafe "casadi__Importer__load_plugin" c_casadi__Importer__load_plugin
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO ()

casadi__Importer__load_plugin
  :: String -> IO ()
casadi__Importer__load_plugin x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Importer__load_plugin errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
importer_load_plugin :: String -> IO ()
importer_load_plugin = casadi__Importer__load_plugin


-- direct wrapper
foreign import ccall unsafe "casadi__Importer__plugin_name" c_casadi__Importer__plugin_name
  :: Ptr (Ptr StdString) -> Ptr Importer' -> IO (Ptr StdString)

casadi__Importer__plugin_name
  :: Importer -> IO String
casadi__Importer__plugin_name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Importer__plugin_name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
importer_plugin_name :: ImporterClass a => a -> IO String
importer_plugin_name x = casadi__Importer__plugin_name (castImporter x)


-- direct wrapper
foreign import ccall unsafe "casadi__Importer__type_name" c_casadi__Importer__type_name
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__Importer__type_name
  :: IO String
casadi__Importer__type_name  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Importer__type_name errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
importer_type_name :: IO String
importer_type_name = casadi__Importer__type_name

