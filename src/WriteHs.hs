{-# OPTIONS_GHC -Wall #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}

module WriteHs ( writeModule
               , writeFunction
               , writeClass
               ) where

import Data.Char ( toLower )
import Data.List ( intersperse )
import Types
import CasadiTree
import qualified Foreign.C.Types as C
import qualified Foreign.Ptr as F


toLower' :: String -> String
toLower' [] = []
toLower' (x:xs) = toLower x:xs

data StdStr = StdStr
marshallStr :: String -> IO (F.Ptr StdStr)
marshallStr = undefined

freeStr :: F.Ptr StdStr -> IO ()
freeStr = undefined

c_boo :: F.Ptr StdStr -> F.Ptr StdStr -> IO C.CInt
c_boo = undefined

boo :: String -> String -> IO C.CInt
boo x0' x1' = do
  withMarshall x0' $ \x0 ->
    withMarshall x1' $ \x1 ->
    c_boo x0 x1

withMarshall :: Marshall a b => a -> (b -> IO c) -> IO c
withMarshall x' f = do
  x <- marshall x'
  ret <- f x
  unmarshall x
  return ret

class Marshall a b | b -> a, a -> b where
  marshall :: a -> IO b
  unmarshall :: b -> IO ()
  unmarshall _ = return ()

instance Marshall Int C.CInt where
  marshall x = return (fromIntegral x)
instance Marshall String (F.Ptr StdStr) where
  marshall x = marshallStr x
  unmarshall = freeStr

writeFunction :: Function -> String
writeFunction (Function (Name functionName) retType params) =
  unlines $
  [ "-- ================== " ++ "function: " ++ show functionName ++ " ==============="
  , "-- functionName: " ++ show functionName
  , "-- cFunctionName: " ++ show cFunctionName
  , "-- hsFunctionName: " ++ show hsFunctionName
  , "-- c_hsFunctionName: " ++ show c_hsFunctionName
  , foreignImport
  , hsFunctionName ++ "\n  :: " ++ proto
  , hsFunctionName ++ " " ++ concat (intersperse " " args) ++ " = undefined"
  ]
  where
    args = take (length params) ["x"++show k ++ "'" | k <- [(0::Int)..]]
--    nonSelfArgs = take (length (fArgs fcn)) ["x" ++ show k | k <- [(0::Int)..]]
--    appArgs
--      | static = concat (intersperse " " nonSelfArgs)
--      | otherwise = concat (intersperse " " ("self" : nonSelfArgs))
--    patternMatchArgs = concat (intersperse " " (self : nonSelfArgs))
--      where
--        self = if static then "_" else "self"
--
--    hsClass = hsName ++ "_Class"
    hsFunctionName = toLower' cFunctionName
    cFunctionName = toCName functionName
    c_hsFunctionName = "c_" ++ cFunctionName
--
--    hsFunctionName = toLower' functionName
    foreignImport = 
      "foreign import ccall unsafe \"" ++ cFunctionName ++ "\" " ++ c_hsFunctionName ++ "\n  :: " ++ ffiProto
----    cname = toCName $ classname ++ "::" ++ name
--    c_hsName = "c_" ++ cName
----    proto
----      | static == Static False = concat (intersperse " -> " hsargs)
----      | otherwise = concat (intersperse " -> " (("Ptr "++classname):hsargs))
    ffiProto = concat $ intersperse " -> " $
               map ffiType params ++ [writeFfiRetType retType]
    proto = concat $ intersperse " -> " $
            map hsType params ++ [writeRetType retType]
--    proto = concat (intersperse " -> " ("a":map hsType (fArgs fcn) ++ [writeRetType (fType fcn)]))
--
    writeRetType :: RetType -> String
    writeRetType (SimpleType x) = "IO " ++ hsType' True x
    writeRetType (NewRef x) = "IO " ++ hsType' True (Ptr x)

    writeFfiRetType :: RetType -> String
    writeFfiRetType (SimpleType x) = "IO " ++ ffiType' True x
    writeFfiRetType (NewRef x) = "IO " ++ ffiType' True (Ptr x)

----    hsArgs
----      | fStatic fcn == Static True = [map hsType (fParams f)]
----      | otherwise = "a"
----      | otherwise = concat (intersperse " -> " (("Ptr "++classname):hsargs))
----    hsargs = map hsType args ++ [hsType' retType]
--
----    defaultFunction = "    " ++ functionName ++ " = i'm a default function, wooo"
--   
---- 
----     proto = cppRetType ++ " " ++ cName ++ protoArgs
----     cppRetType = cppType $ fromRetType (fType fcn)
----     cName = toCName cppName
--    static = if fStatic fcn == Static True then True else False
--    static' = if static then "static " else ""
--    Name functionName = fName fcn
----    classname = cppType classType
----    protoArgs = "(" ++ intercalate ", " allProtoArgs ++ ")"
----    nonSelfProtoArgs = map (uncurry paramProto) $ zip [0..] (fArgs fcn)
----    allProtoArgs
----      | fStatic fcn == Static True = nonSelfProtoArgs
----      | otherwise = (cppType (Ptr class') ++ " obj") : nonSelfProtoArgs
----    args = "(" ++ intercalate ", " (map (paramName . fst) $ zip [0..] (fArgs fcn)) ++ ")"
----    call
----      | fStatic fcn == Static True = cppName ++ args
----      | otherwise = "obj->" ++ functionName ++ args

writeClass :: Class -> String
writeClass (Class classType methods) =
  unlines $
  --foreignImports ++
  [ ""
  , "class " ++ hsClass ++ " a where"
  , "    coerce_" ++ hsName ++ " :: a -> " ++ hsName
--  ] ++ classMethods ++
--  [ ""
  , "newtype " ++ hsName ++ " = " ++ hsName ++ " (Ptr Raw)"
  , "instance " ++ hsClass ++ " " ++ hsName ++ " where"
  , "    coerce_" ++ hsName ++ " = id"
  ]
  where
    hsClass = hsName ++ "_Class"
    hsName = hsType classType

    --(classMethods, foreignImports) = unzip $ map (writeMethod classType) methods

writeMethod :: Type -> Method -> (String, String)
writeMethod classType fcn = (method, foreignImport)
  where
    method =
      unlines $ map ("    " ++) $
      [ "-- ================== " ++ static' ++ "method: " ++ show methodName ++ " ==============="
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
      , default'
--              --  , show (name, retType, args, const', static)
      ]

    default' = hsMethodName ++ " " ++ patternMatchArgs ++ " = " ++ c_hsName ++ " " ++ appArgs
    nonSelfArgs = take (length (fArgs fcn)) ["x" ++ show k | k <- [(0::Int)..]]
    appArgs
      | static = concat (intersperse " " nonSelfArgs)
      | otherwise = concat (intersperse " " ("self" : nonSelfArgs))
    patternMatchArgs = concat (intersperse " " (self : nonSelfArgs))
      where
        self = if static then "_" else "self"

    hsClass = hsName ++ "_Class"
    hsName = toLower' $ hsType classType
    cppName = cppType classType ++ "::" ++ methodName
    cName = toCName cppName

    hsMethodName = toLower' methodName
    foreignImport = 
      "foreign import ccall unsafe \"" ++ cName ++ "\" " ++ c_hsName ++ "\n      :: " ++ ffiProto
--    cname = toCName $ classname ++ "::" ++ name
    c_hsName = "c_" ++ cName
--    proto
--      | static == Static False = concat (intersperse " -> " hsargs)
--      | otherwise = concat (intersperse " -> " (("Ptr "++classname):hsargs))
    ffiProto
      | static = concat (intersperse " -> " ffiNonselfArgs)
      | otherwise = concat (intersperse " -> " ("a":ffiNonselfArgs))
    ffiNonselfArgs = map hsType (fArgs fcn) ++ [writeRetType (fType fcn)]
    proto = concat (intersperse " -> " ("a":map hsType (fArgs fcn) ++ [writeRetType (fType fcn)]))

    writeRetType :: RetType -> String
    writeRetType (SimpleType x) = "IO " ++ hsType' True x
    writeRetType (NewRef x) = "IO " ++ hsType' True (Ptr x)

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
    static = if fStatic fcn == Static True then True else False
    static' = if static then "static " else ""
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
  , "module " ++ moduleName ++ " where"
  , ""
  , "import Data.Vector ( Vector )"
  , "import Foreign.Ptr ( Ptr )"
  , ""
  , "data Raw = Raw"
  ]
  ++ map writeClass classes ++ map writeFunction functions

main :: IO ()
main = do
  putStrLn $ writeModule "Test" [sxfun, sxmat] tools
--  mapM_ putStrLn $ map writeClass [fx]

