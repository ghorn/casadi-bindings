{-# OPTIONS_GHC -Wall #-}

module WriteBindings.WriteCTools
       ( cToolsImports
       , cToolsInstances
       ) where

writeStuff :: (String, String, Maybe String, [String]) -> (String,String)
writeStuff woo@(cname, hsname, hstype', hsToCs) = (foreignImports', instances')
  where
    foreignImports' =
      unlines
      [ "\n--------------------------- " ++ show woo ++ " -----------------------------------"
      , "foreign import ccall unsafe \"hs_new_vec_" ++ cname ++ "\" c_newVec" ++ hsname
      , "  :: Ptr " ++ hstype ++ " -> CInt -> IO (Ptr (CppVec " ++ hstype ++ "))"
      , "foreign import ccall unsafe \"hs_delete_vec_" ++ cname ++ "\" c_deleteVec" ++ hsname
      , "  :: Ptr (CppVec " ++ hstype ++ ") -> IO ()"
      , "foreign import ccall unsafe \"hs_copy_vec_" ++ cname ++ "\" c_copyVec" ++ hsname
      , "  :: Ptr (CppVec " ++ hstype ++ ") -> Ptr " ++ hstype ++ " -> IO ()"
      , "foreign import ccall unsafe \"hs_size_vec_" ++ cname ++ "\" c_sizeVec" ++ hsname
      , "  :: Ptr (CppVec " ++ hstype ++ ") -> IO CInt"
      ]
    instances' =
      unlines
      [ "\n-- "++ show woo
      , "instance Marshal (V.Vector " ++ hstype ++ ") (Ptr (CppVec " ++ hstype ++ ")) where"
      , "  marshal = newStorableVec c_newVec" ++ hsname
      , "  marshalFree = const c_deleteVec" ++ hsname
      ] ++ concatMap m hsToCs

    m x = unlines
          [ "instance Marshal (V.Vector " ++ x ++ ") (Ptr (CppVec " ++ hstype ++ ")) where"
          , "  marshal = newStorableVec c_newVec" ++ hsname ++ " . (V.map hsToC)"
          , "  marshalFree = const c_deleteVec" ++ hsname
          ]

    hstype = case hstype' of Nothing -> hsname
                             Just x -> x

foreignImports, instances :: [String]
(foreignImports, instances) =
  unzip $ map writeStuff
  [ ("int","CInt",Nothing, ["Int","Bool"])
  , ("voidp","VoidP",Just "(Ptr a)", [])
  , ("uchar","CUChar",Nothing, [])
  , ("double","CDouble",Nothing, ["Double"])
  ]

cToolsImports :: String
cToolsImports =
  unlines
  [ "{-# OPTIONS_GHC -Wall #-}"
  , "{-# Language ForeignFunctionInterface #-}"
  , ""
  , "module Casadi.Wrappers.CToolsImports where"
  , ""
  , "import Foreign.C.Types"
  , "import Foreign.Ptr ( Ptr )"
  , ""
  , "import Casadi.MarshalTypes"
  ] ++ concat foreignImports

cToolsInstances :: String
cToolsInstances =
  unlines
  [ "{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}"
  , "{-# Language FlexibleInstances #-}"
  , "{-# Language MultiParamTypeClasses #-}"
  , ""
  , "module Casadi.Wrappers.CToolsInstances where"
  , ""
  , "import Foreign.C.Types"
  , "import Foreign.Ptr ( Ptr )"
  , "import qualified Data.Vector as V"
  , ""
  , "import Casadi.MarshalTypes"
  , "import Casadi.Marshal"
  , "import Casadi.Wrappers.CToolsImports"
  , ""
  ] ++ concat instances
