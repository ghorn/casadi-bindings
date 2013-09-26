{-# OPTIONS_GHC -Wall #-}

module WriteHs ( writeModule
               , writeFunction
               , writeClass
               ) where

import Data.Char ( toLower )
import Data.List ( intersperse )
import Types

-- haskell functions can't have capital leading letter
beautifulHaskellName :: String -> String
beautifulHaskellName [] = error "beautifulHaskellName: empty string will never be beautiful :'("
beautifulHaskellName "SXFunction" = "sxFunction"
-- fall back on just making just the first letter lowercase
beautifulHaskellName (x:xs) = toLower x:xs

marshallFun :: Int -> String -> String -> (String, String)
marshallFun n fun wrappedFun =
  (fun ++ " " ++ concat (intersperse " " args) ++ " =\n" ++ marshalls,
   wrappedFun ++ " " ++ concat (intersperse " " args'))
  where
    ks = take n [(0::Int)..]
    marshalls = unlines $ blah args
    args = ["x"++show k | k <- ks]
    args' = ["x"++show k++"'" | k <- ks]

    blah :: [String] -> [String]
    blah (x:xs) = ("  withMarshall " ++ x ++ " $ \\" ++ x ++ "' ->"):blah xs
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
  , marshalls ++ "  " ++ call
  ]
  where
    (marshalls, call') = marshallFun (length args) hsFunctionName c_hsFunctionName
    call = case makesNewRef retType of
      Nothing -> call' ++ " >>= wrapReturn"
      Just v -> call' ++ " >>= (newForeignPtr " ++ c_deleteName v ++ ") >>= wrapReturn"

    args = take (length params) ["x"++show k ++ "'" | k <- [(0::Int)..]]
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
  , "instance Marshall " ++ hsName ++ " (ForeignPtr " ++ hsName ++ "') where"
  , "    withMarshall ("++ hsName ++ " x) f = f x"
  , "instance Marshall " ++ hsName ++ " (Ptr " ++ hsName ++ "') where"
  , "    withMarshall ("++ hsName ++ " x) f = withMarshall x f"
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
  , "module Gen." ++ moduleName ++ " where"
  , ""
  , "import Data.Vector ( Vector )"
  , "import Foreign.C.Types"
  , "import Foreign.C.String"
  , "import Foreign.Ptr ( FunPtr, Ptr )"
  , "import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr )"
  , "import Marshall ( ForeignPtrWrapper(..), Marshall(..), WrapReturn(..), CppVec, CppVecVec, CppVecVecVec )"
  , ""
  ] ++ map deleteForeignImports [CInt,CDouble,StdString] ++
  map writeClass classes ++ map writeFunction functions
