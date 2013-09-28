{-# OPTIONS_GHC -Wall #-}

module CasadiBindings.WriteForeignTools
       ( main
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
      , "foreign import ccall unsafe \"hs_new_vec_vec_" ++ cname ++ "\" c_newVecVec" ++ hsname
      , "  :: Ptr " ++ hstype ++ " -> CInt -> Ptr CInt -> IO (Ptr (CppVecVec " ++ hstype ++ "))"
      , "foreign import ccall unsafe \"hs_delete_vec_vec_" ++ cname ++ "\" c_deleteVecVec" ++ hsname
      , "  :: Ptr (CppVecVec " ++ hstype ++ ") -> IO ()"
      ]
    instances' =
      unlines
      [ "instance Marshal (V.Vector " ++ hstype ++ ") (Ptr (CppVec " ++ hstype ++ ")) where"
      , "  withMarshal = withMarshalStorableVec c_newVec" ++ hsname ++ " c_deleteVec" ++ hsname
      ] ++ concatMap m hsToCs

    m x = unlines
          [ "instance Marshal (V.Vector " ++ x ++ ") (Ptr (CppVec " ++ hstype ++ ")) where"
          , "  withMarshal x = withMarshalStorableVec c_newVec" ++ hsname ++ " c_deleteVec" ++ hsname ++
            " (V.map hsToC x)"
          , "instance Marshal (V.Vector (V.Vector " ++ x ++ ")) (Ptr (CppVecVec " ++ hstype ++ ")) where"
          , "  withMarshal x = withMarshalStorableVecVec c_newVecVec" ++ hsname ++ " c_deleteVecVec" ++ hsname ++
            " (V.map (V.map hsToC) x)"
          ]

    hstype = case hstype' of Nothing -> hsname
                             Just x -> x

foreignImports, instances :: [String]
(foreignImports, instances) =
  unzip $ map writeStuff
  [ ("int","CInt",Nothing, ["Int"])
  , ("voidp","VoidP",Just "(Ptr a)", [])
  , ("uchar","CUChar",Nothing, [])
  , ("double","CDouble",Nothing, ["Double"])
  ]

foreignToolsImports :: String
foreignToolsImports =
  unlines
  [ "{-# OPTIONS_GHC -Wall #-}"
  , "{-# Language ForeignFunctionInterface #-}"
  , ""
  , "module CasadiBindings.Gen.ForeignToolsImports where"
  , ""
  , "import Foreign.C.Types"
  , "import Foreign.Ptr ( Ptr )"
  , ""
  , "import CasadiBindings.MarshalTypes"
  ] ++ concat foreignImports

foreignToolsInstances :: String
foreignToolsInstances =
  unlines
  [ "{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}"
  , "{-# Language FlexibleInstances #-}"
  , "{-# Language MultiParamTypeClasses #-}"
  , ""
  , "module CasadiBindings.Gen.ForeignToolsInstances where"
  , ""
  , "import Foreign.C.Types"
  , "import Foreign.Ptr ( Ptr )"
  , "import qualified Data.Vector as V"
  , ""
  , "import CasadiBindings.MarshalTypes"
  , "import CasadiBindings.Marshal"
  , "import CasadiBindings.Gen.ForeignToolsImports"
  , ""
  ] ++ concat instances

main :: IO ()
main = do
  writeFile "Gen/ForeignToolsImports.hs" foreignToolsImports
  writeFile "Gen/ForeignToolsInstances.hs" foreignToolsInstances
