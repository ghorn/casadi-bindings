{-# OPTIONS_GHC -Wall #-}

module WriteCasadiBindings.WriteHs ( writeModule
                                   , writeFunction
                                   , writeClass
                                   ) where

import Data.Char ( toLower )
import Data.List ( intersperse )

import WriteCasadiBindings.Types
import WriteCasadiBindings.TypeMaps

-- haskell functions can't have capital leading letter
beautifulHaskellName :: String -> String
beautifulHaskellName [] = error "beautifulHaskellName: empty string will never be beautiful :'("
beautifulHaskellName "SXFunction" = "sxFunction"
-- fall back on just making just the first letter lowercase
beautifulHaskellName (x:xs) = toLower x:xs

marshalFun :: [Type] -> String -> String -> (String, String)
marshalFun params fun wrappedFun =
  (fun ++ " " ++ concat (intersperse " " patternMatchArgs) ++ " =\n" ++ marshals,
   wrappedFun ++ " " ++ concat (intersperse " " appArgs))
  where
    marshals = unlines $ blah $ args
    args = zipWith (\k p -> ("x"++show k, p)) [(0::Int)..] params
    patternMatchArgs = map fst args
    appArgs = map (++ "'") patternMatchArgs

    blah :: [(String,Type)] -> [String]
    blah ((x,t):xs) = arg:blah xs
      where
        arg = case hsMarshalNewtypeWrapper t of
          Nothing -> "  withMarshal " ++ x ++ " $ \\" ++ x ++ "' ->"
          Just ntw -> "  withMarshal (" ++ ntw ++ " " ++ x ++ ") $ \\" ++ x ++ "' ->"
    blah [] = []

writeFunction :: Function -> String
writeFunction fcn@(Function (Name functionName) retType params) =
  unlines $
  [ "-- ================== " ++ "function: " ++ show functionName ++ " ==============="
  , "-- functionName: " ++ show functionName
  , "-- cFunctionName: " ++ show cFunctionName
  , "-- hsFunctionName: " ++ show hsFunctionName
  , "-- c_hsFunctionName: " ++ show c_hsFunctionName
  , "-- retType': " ++ show retType'
  , "-- retType: " ++ show retType
  , "-- params: " ++ show params
  , "-- map ffiType params: " ++ show (map (ffiType False) params)
  , "-- ffiRetType: " ++ show ffiRetType
  , foreignImport
  , hsFunctionName ++ "\n  :: " ++ proto
  , marshals ++ "  " ++ call
  ]
  where
    (marshals, call') = marshalFun params hsFunctionName c_hsFunctionName
    call = case makesNewRef retType of
      Nothing -> call' ++ " >>= wrapReturn"
      Just v -> call' ++ " >>= (newForeignPtr " ++ c_deleteName v ++ ") >>= wrapReturn"

    hsFunctionName = beautifulHaskellName cFunctionName
    cFunctionName = cWrapperName' fcn
    c_hsFunctionName = "c_" ++ cFunctionName
    foreignImport =
      "foreign import ccall unsafe \"" ++ cFunctionName ++ "\" " ++ c_hsFunctionName ++ "\n  :: " ++ ffiProto
    ffiProto = concat $ intersperse " -> " $
               map (ffiType False) params ++ [ffiRetType]
    proto = concat $ intersperse " -> " $
            map (hsType False) params ++ [retType']


    retType' :: String
    retType' = "IO " ++ hsType True retType

    ffiRetType :: String
    ffiRetType = "IO " ++ ffiType True retType

c_deleteName :: ThreeVectors -> String
c_deleteName = ("c_" ++) .  deleteName

deleteForeignImports :: Primitive -> String
deleteForeignImports classType = concatMap writeIt types
  where
    types = [ NonVec classType
            , Vec (NonVec classType)
            , Vec (Vec (NonVec classType))
            , Vec (Vec (Vec (NonVec classType)))
            ]
    writeIt c =
      unlines $
      [ "foreign import ccall unsafe \"&" ++ deleteName c ++ "\" "
      , "  " ++ c_deleteName c ++ " :: FunPtr ("++ ffiTypeTV False c ++ " -> IO ())"
      ]


writeClass :: Class -> String
writeClass (Class classType methods) =
  unlines $
  ffiWrappers ++
  [ ""
  , "--class " ++ hsClass ++ " a where"
--  , "--    coerce_" ++ hsName ++ " :: a -> " ++ hsName
  ] ++ -- classMethods ++
  [ ""
  , deleteForeignImports (CasadiClass classType)
  , "data " ++ hsName ++ "'"
  , "newtype " ++ hsName ++ " = " ++ hsName ++ " (ForeignPtr " ++ hsName ++ "')"
  , "instance Marshal " ++ hsName ++ " (ForeignPtr " ++ hsName ++ "') where"
  , "    withMarshal ("++ hsName ++ " x) f = f x"
  , "instance Marshal " ++ hsName ++ " (Ptr " ++ hsName ++ "') where"
  , "    withMarshal ("++ hsName ++ " x) f = withMarshal x f"
  , "instance ForeignPtrWrapper " ++ hsName ++ " " ++ hsName ++ "' where"
  , "    unwrapForeignPtr ("++ hsName ++ " x) = x"
  , "instance WrapReturn (ForeignPtr " ++ hsName ++ "') " ++ hsName ++ " where"
  , "    wrapReturn = return . " ++ hsName
  ]
  where
    hsClass = hsName ++ "_Class"
    hsName = hsTypePrim (CasadiClass classType)

    (_, ffiWrappers) = unzip $ map (writeMethod classType) methods

writeMethod :: CasadiClass -> Method -> (String, String)
writeMethod classType fcn = (method, ffiWrapper)
  where
    method =
      unlines $ map ("    " ++) $
      [ "-- ================== " ++ show (fMethodType fcn) ++ " method: "
        ++ show methodName ++ " ==============="
      , "-- class: " ++ show hsClass
      , "-- hsname: " ++ show hsName
      , "-- cppName: " ++ show cppName
      , "-- cWrapperName: " ++ show cWrapperName''
      , "-- methodName: " ++ show methodName
      , "-- hsMethodName: " ++ show hsMethodName
      , "-- proto: " ++ show proto
      , hsMethodName ++ "\n      :: " ++ proto
      ]
    hsClass = hsName ++ "_Class"
    hsName = beautifulHaskellName $ hsTypePrim (CasadiClass classType)
    cppName = cppMethodName classType fcn
    cWrapperName'' = cWrapperName classType fcn

    hsMethodName = beautifulHaskellName methodName
    -- this hack might not quite give the right name
    ffiWrapper = case fMethodType fcn of
      Normal -> writeFunction $ Function (Name cWrapperName'') (fType fcn) ((Ref (NonVec (CasadiClass classType))):(fArgs fcn))
      _ -> writeFunction (Function (Name cWrapperName'') (fType fcn) (fArgs fcn))

    proto = concat (intersperse " -> " ("a":map (hsType False) (fArgs fcn) ++ [retType']))
    retType' = "IO " ++ hsType True (fType fcn)

    Name methodName = fName fcn





writeModule :: String -> [Class] -> [Function] -> String
writeModule moduleName classes functions =
  init $ unlines $
  [ "{-# OPTIONS_GHC -Wall #-}"
  , "{-# Language ForeignFunctionInterface #-}"
  , "{-# Language FlexibleInstances #-}"
  , "{-# Language MultiParamTypeClasses #-}"
  , ""
  , "module CasadiBindings.Gen." ++ moduleName ++ " where"
  , ""
  , "import Data.Vector ( Vector )"
  , "import Foreign.C.Types"
  , "import Foreign.C.String"
  , "import Foreign.Ptr ( FunPtr, Ptr )"
  , "import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr )"
  , "import CasadiBindings.MarshalTypes ( ForeignPtrWrapper(..), CppVec, CppVecVec, CppVecVecVec,"
  , "                                     StdString', StdOstream', CppBool' )"
  , "import CasadiBindings.Marshal (  CornerCase(..), Marshal(..) )"
  , "import CasadiBindings.WrapReturn ( WrapReturn(..) )"
  , "import CasadiBindings.Gen.ForeignToolsInstances"
  , ""
  ] ++ map deleteForeignImports [CInt,CDouble,StdString,CBool] ++
  map writeClass classes ++ map writeFunction functions
