{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Internal.CToolsInstances where

import Foreign.C.Types
import Foreign.Ptr ( Ptr )
import qualified Data.Vector as V

import Casadi.Internal.MarshalTypes
import Casadi.Internal.Marshal
import Casadi.Internal.CToolsImports


-- ("int","CInt",Nothing,["Int","Bool"])
instance Marshal (V.Vector CInt) (Ptr (StdVec CInt)) where
  marshal = newStorableVec c_newVecCInt
  marshalFree = const c_deleteVecCInt
instance Marshal (V.Vector Int) (Ptr (StdVec CInt)) where
  marshal = newStorableVec c_newVecCInt . (V.map hsToC)
  marshalFree = const c_deleteVecCInt
instance Marshal (V.Vector Bool) (Ptr (StdVec CInt)) where
  marshal = newStorableVec c_newVecCInt . (V.map hsToC)
  marshalFree = const c_deleteVecCInt

-- ("voidp","VoidP",Just "(Ptr a)",[])
instance Marshal (V.Vector (Ptr a)) (Ptr (StdVec (Ptr a))) where
  marshal = newStorableVec c_newVecVoidP
  marshalFree = const c_deleteVecVoidP

-- ("uchar","CUChar",Nothing,[])
instance Marshal (V.Vector CUChar) (Ptr (StdVec CUChar)) where
  marshal = newStorableVec c_newVecCUChar
  marshalFree = const c_deleteVecCUChar

-- ("double","CDouble",Nothing,["Double"])
instance Marshal (V.Vector CDouble) (Ptr (StdVec CDouble)) where
  marshal = newStorableVec c_newVecCDouble
  marshalFree = const c_deleteVecCDouble
instance Marshal (V.Vector Double) (Ptr (StdVec CDouble)) where
  marshal = newStorableVec c_newVecCDouble . (V.map hsToC)
  marshalFree = const c_deleteVecCDouble
