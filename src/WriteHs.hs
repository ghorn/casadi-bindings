{-# OPTIONS_GHC -Wall #-}

module WriteHs ( writeModule
               , writeFunction
               , writeClass
               ) where

import Data.Char ( toLower )
import Data.List ( intersperse )
import Types
import CasadiTree
import WriteC ( deleteName )

-- haskell functions can't have capital leading letter
beautifulHaskellName :: String -> String
beautifulHaskellName [] = error "beautifulHaskellName: empty string will never be beautiful :'("
beautifulHaskellName "SXFunction" = "sxFunction"
-- fall back on just making just the first letter lowercase
beautifulHaskellName (x:xs) = toLower x:xs

marshallFun :: Int -> String -> String -> String
marshallFun n fun wrappedFun =
  fun ++ " " ++ concat (intersperse " " args) ++ " =\n" ++
  marshalls ++ "  " ++ wrappedFun ++ " " ++ concat (intersperse " " args')
  where
    ks = take n [(0::Int)..]
    marshalls = unlines $ blah args
    args = ["x"++show k | k <- ks]
    args' = ["x"++show k++"'" | k <- ks]

    blah :: [String] -> [String]
    blah (x:xs) = ("  withMarshall " ++ x ++ " $ \\" ++ x ++ "' ->"):blah xs
    blah [] = []

writeFunction :: Function -> String
writeFunction (Function (Name functionName) retType params) =
  unlines $
  [ "-- ================== " ++ "function: " ++ show functionName ++ " ==============="
  , "-- functionName: " ++ show functionName
  , "-- cFunctionName: " ++ show cFunctionName
  , "-- hsFunctionName: " ++ show hsFunctionName
  , "-- c_hsFunctionName: " ++ show c_hsFunctionName
  , "-- retType': " ++ show retType'
  , "-- retType: " ++ show retType
  , "-- params: " ++ show params
  , "-- map ffiType params: " ++ show (map ffiType params)
  , "-- ffiRetType: " ++ show ffiRetType
  , foreignImport
  , hsFunctionName ++ "\n  :: " ++ proto
  , marshallFun (length args) hsFunctionName c_hsFunctionName ++ newFinalizer
  ]
  where
    args = take (length params) ["x"++show k ++ "'" | k <- [(0::Int)..]]
    hsFunctionName = beautifulHaskellName cFunctionName
    cFunctionName = toCName functionName
    c_hsFunctionName = "c_" ++ cFunctionName
    foreignImport = 
      "foreign import ccall unsafe \"" ++ cFunctionName ++ "\" " ++ c_hsFunctionName ++ "\n  :: " ++ ffiProto
    ffiProto = concat $ intersperse " -> " $
               map ffiType params ++ [ffiRetType]
    proto = concat $ intersperse " -> " $
            map hsType params ++ [retType']

    newFinalizer = case retType of
      (SimpleType _)-> ""
      (NewRef x) -> " >>= ((fmap "++ hsType (Prim (CP x)) ++ ") . (newForeignPtr " ++ c_deleteName x ++ "))"
    
    retType' :: String
    retType' = case retType of
      (SimpleType x) -> "IO " ++ hsType' True x
      (NewRef x) -> "IO " ++ hsType' True (Ptr (CP x))

    ffiRetType :: String
    ffiRetType = case retType of
      (SimpleType x) -> "IO " ++ ffiType' True x
      (NewRef x) -> "IO " ++ ffiType' True (Prim (CP x))

c_deleteName :: CasadiPrimitive -> String
c_deleteName = ("c_" ++) .  deleteName

writeClass :: Class -> String
writeClass (Class classType methods) =
  unlines $
  ffiWrappers ++
  [ ""
  , "class " ++ hsClass ++ " a where"
  , "    coerce_" ++ hsName ++ " :: a -> " ++ hsName
  ] ++ classMethods ++
  [ ""
  , "foreign import ccall unsafe \"&" ++ deleteName classType ++ "\" "
    ++ c_deleteName classType ++ " :: FunPtr ("++ ffiType (Prim (CP classType)) ++ " -> IO ())"
  , "data " ++ hsName ++ "'"
  , "newtype " ++ hsName ++ " = " ++ hsName ++ " (ForeignPtr " ++ hsName ++ "')"
  , "instance Marshall " ++ hsName ++ " (ForeignPtr " ++ hsName ++ "') where"
  , "    withMarshall ("++ hsName ++ " x) f = f x"
  , "instance Marshall " ++ hsName ++ " (Ptr " ++ hsName ++ "') where"
  , "    withMarshall ("++ hsName ++ " x) f = withMarshall x f"
  , "instance ForeignPtrWrapper " ++ hsName ++ " " ++ hsName ++ "' where"
  , "    unwrapForeignPtr ("++ hsName ++ " x) = x"
  , "--instance " ++ hsClass ++ " " ++ hsName ++ " where"
  , "--    coerce_" ++ hsName ++ " = id"
  ]
  where
    hsClass = hsName ++ "_Class"
    hsName = hsType (Prim (CP classType))

    (classMethods, ffiWrappers) = unzip $ map (writeMethod classType) methods

writeMethod :: CasadiPrimitive -> Method -> (String, String)
writeMethod classType fcn = (method, ffiWrapper)
  where
    method =
      unlines $ map ("    " ++) $
      [ "-- ================== " ++ show (fMethodType fcn) ++ " method: "
        ++ show methodName ++ " ==============="
      , "-- class: " ++ show hsClass
      , "-- hsname: " ++ show hsName
      , "-- cppName: " ++ show cppName
      , "-- cName: " ++ show cName
      , "-- methodName: " ++ show methodName
      , "-- hsMethodName: " ++ show hsMethodName
--      , "-- protoArgs: " ++ show protoArgs
--      , "-- args: " ++ show args
--      , "-- rettype: " ++ show (fType fcn)
--      , "-- rettype-cpp: " ++ show cppRetType
      , "-- proto: " ++ show proto
--      , "-- call: " ++ show call
--      , ""
--      , "extern \"C\"\n    " ++ proto ++ ";"
--      , proto ++ "{"
--      , writeReturn (fType fcn) call
--      , "}"
--      , ""
      , hsMethodName ++ "\n      :: " ++ proto
--      , default'
--              --  , show (name, retType, args, const', static)
      ]

--    default' = hsMethodName ++ " " ++ patternMatchArgs ++ " = " ++ c_hsName ++ " " ++ appArgs
    nonSelfArgs = take (length (fArgs fcn)) ["x" ++ show k | k <- [(0::Int)..]]
    appArgs = case fMethodType fcn of
      Normal -> concat (intersperse " " ("self" : nonSelfArgs))
      _ -> concat (intersperse " " nonSelfArgs)
--    patternMatchArgs = concat (intersperse " " (self : nonSelfArgs))
--      where
--        self = if static then "_" else "self"

    hsClass = hsName ++ "_Class"
    hsName = beautifulHaskellName $ hsType (Prim (CP classType))
    cppName = cppType (Prim (CP classType)) ++ "::" ++ methodName
    cName = toCName cppName

    hsMethodName = beautifulHaskellName methodName
    ffiWrapper = case fMethodType fcn of
      Normal -> writeFunction (Function (Name cppName) (fType fcn) (Ptr (CP classType):(fArgs fcn)))
      _ -> writeFunction (Function (Name cppName) (fType fcn) (fArgs fcn))

    proto = concat (intersperse " -> " ("a":map hsType (fArgs fcn) ++ [writeRetType (fType fcn)]))

    writeRetType :: RetType -> String
    writeRetType (SimpleType x) = "IO " ++ hsType' True x
    writeRetType (NewRef x) = "IO " ++ hsType' True (Ptr (CP x))

--    hsArgs
--      | fStatic fcn == Static True = [map hsType (fParams f)]
--      | otherwise = "a"
--      | otherwise = concat (intersperse " -> " (("Ptr "++classname):hsargs))
--    hsargs = map hsType args ++ [hsType' retType]

--    defaultMethod = "    " ++ methodName ++ " = i'm a default method, wooo"
   
-- 
--     proto = cppRetType ++ " " ++ cName ++ protoArgs
--     cppRetType = cppType $ fromRetType (fType fcn)
--     cName = toCName cppName
    Name methodName = fName fcn
--    classname = cppType classType
--    protoArgs = "(" ++ intercalate ", " allProtoArgs ++ ")"
--    nonSelfProtoArgs = map (uncurry paramProto) $ zip [0..] (fArgs fcn)
--    allProtoArgs
--      | fStatic fcn == Static True = nonSelfProtoArgs
--      | otherwise = (cppType (Ptr class') ++ " obj") : nonSelfProtoArgs
--    args = "(" ++ intercalate ", " (map (paramName . fst) $ zip [0..] (fArgs fcn)) ++ ")"
--    call
--      | fStatic fcn == Static True = cppName ++ args
--      | otherwise = "obj->" ++ methodName ++ args

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
  , "import Marshall ( ForeignPtrWrapper(..), Marshall(..) )"
  , ""
  ]
  ++ map writeClass classes ++ map writeFunction functions

main :: IO ()
main = do
  putStrLn $ writeModule "Test" [sxfun, sxmat] tools
--  mapM_ putStrLn $ map writeClass [fx]

