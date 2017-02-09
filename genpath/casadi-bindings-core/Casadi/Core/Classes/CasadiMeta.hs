{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.CasadiMeta
       (
         CasadiMeta,
         CasadiMetaClass(..),
         casadiMeta_getBuildType,
         casadiMeta_getCompiler,
         casadiMeta_getCompilerFlags,
         casadiMeta_getCompilerId,
         casadiMeta_getFeatureList,
         casadiMeta_getGitDescribe,
         casadiMeta_getGitRevision,
         casadiMeta_getInstallPrefix,
         casadiMeta_getModules,
         casadiMeta_getPlugins,
         casadiMeta_getVersion,
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
foreign import ccall unsafe "casadi__CasadiMeta__getBuildType" c_casadi__CasadiMeta__getBuildType
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__CasadiMeta__getBuildType
  :: IO String
casadi__CasadiMeta__getBuildType  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CasadiMeta__getBuildType errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
casadiMeta_getBuildType :: IO String
casadiMeta_getBuildType = casadi__CasadiMeta__getBuildType


-- direct wrapper
foreign import ccall unsafe "casadi__CasadiMeta__getCompiler" c_casadi__CasadiMeta__getCompiler
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__CasadiMeta__getCompiler
  :: IO String
casadi__CasadiMeta__getCompiler  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CasadiMeta__getCompiler errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
casadiMeta_getCompiler :: IO String
casadiMeta_getCompiler = casadi__CasadiMeta__getCompiler


-- direct wrapper
foreign import ccall unsafe "casadi__CasadiMeta__getCompilerFlags" c_casadi__CasadiMeta__getCompilerFlags
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__CasadiMeta__getCompilerFlags
  :: IO String
casadi__CasadiMeta__getCompilerFlags  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CasadiMeta__getCompilerFlags errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
casadiMeta_getCompilerFlags :: IO String
casadiMeta_getCompilerFlags = casadi__CasadiMeta__getCompilerFlags


-- direct wrapper
foreign import ccall unsafe "casadi__CasadiMeta__getCompilerId" c_casadi__CasadiMeta__getCompilerId
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__CasadiMeta__getCompilerId
  :: IO String
casadi__CasadiMeta__getCompilerId  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CasadiMeta__getCompilerId errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
casadiMeta_getCompilerId :: IO String
casadiMeta_getCompilerId = casadi__CasadiMeta__getCompilerId


-- direct wrapper
foreign import ccall unsafe "casadi__CasadiMeta__getFeatureList" c_casadi__CasadiMeta__getFeatureList
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__CasadiMeta__getFeatureList
  :: IO String
casadi__CasadiMeta__getFeatureList  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CasadiMeta__getFeatureList errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
casadiMeta_getFeatureList :: IO String
casadiMeta_getFeatureList = casadi__CasadiMeta__getFeatureList


-- direct wrapper
foreign import ccall unsafe "casadi__CasadiMeta__getGitDescribe" c_casadi__CasadiMeta__getGitDescribe
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__CasadiMeta__getGitDescribe
  :: IO String
casadi__CasadiMeta__getGitDescribe  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CasadiMeta__getGitDescribe errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
casadiMeta_getGitDescribe :: IO String
casadiMeta_getGitDescribe = casadi__CasadiMeta__getGitDescribe


-- direct wrapper
foreign import ccall unsafe "casadi__CasadiMeta__getGitRevision" c_casadi__CasadiMeta__getGitRevision
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__CasadiMeta__getGitRevision
  :: IO String
casadi__CasadiMeta__getGitRevision  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CasadiMeta__getGitRevision errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
casadiMeta_getGitRevision :: IO String
casadiMeta_getGitRevision = casadi__CasadiMeta__getGitRevision


-- direct wrapper
foreign import ccall unsafe "casadi__CasadiMeta__getInstallPrefix" c_casadi__CasadiMeta__getInstallPrefix
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__CasadiMeta__getInstallPrefix
  :: IO String
casadi__CasadiMeta__getInstallPrefix  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CasadiMeta__getInstallPrefix errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
casadiMeta_getInstallPrefix :: IO String
casadiMeta_getInstallPrefix = casadi__CasadiMeta__getInstallPrefix


-- direct wrapper
foreign import ccall unsafe "casadi__CasadiMeta__getModules" c_casadi__CasadiMeta__getModules
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__CasadiMeta__getModules
  :: IO String
casadi__CasadiMeta__getModules  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CasadiMeta__getModules errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
casadiMeta_getModules :: IO String
casadiMeta_getModules = casadi__CasadiMeta__getModules


-- direct wrapper
foreign import ccall unsafe "casadi__CasadiMeta__getPlugins" c_casadi__CasadiMeta__getPlugins
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__CasadiMeta__getPlugins
  :: IO String
casadi__CasadiMeta__getPlugins  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CasadiMeta__getPlugins errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
casadiMeta_getPlugins :: IO String
casadiMeta_getPlugins = casadi__CasadiMeta__getPlugins


-- direct wrapper
foreign import ccall unsafe "casadi__CasadiMeta__getVersion" c_casadi__CasadiMeta__getVersion
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__CasadiMeta__getVersion
  :: IO String
casadi__CasadiMeta__getVersion  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__CasadiMeta__getVersion errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
casadiMeta_getVersion :: IO String
casadiMeta_getVersion = casadi__CasadiMeta__getVersion

