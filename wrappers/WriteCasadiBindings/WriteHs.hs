{-# OPTIONS_GHC -Wall #-}

module WriteCasadiBindings.WriteHs ( writeClassModules
                                   , writeDeleterModule
                                   , writeDataModule
                                   , writeToolsModule
                                   ) where

import Data.Char ( toLower )
import Data.List ( intersperse, sort )
import Data.Maybe ( fromMaybe )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import WriteCasadiBindings.Types
import WriteCasadiBindings.TypeMaps

replaces :: [(String,String)] -> String -> String
replaces ((find',replace'):xs) = replaces xs . (T.unpack . T.replace (T.pack find') (T.pack replace') . T.pack)
replaces [] = id

prettyTics :: String -> String
prettyTics = replaces [("_TIC","'")]


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

c_deleteName :: ThreeVectors -> String
c_deleteName = ("c_" ++) .  deleteName

exportDecl :: [String] -> String
exportDecl names =
  unlines $
  "       (" :
  map (\x -> "         " ++ x ++ ",") names ++
  ["       ) where"]


deleters :: [Class] -> [String]
deleters classes = map deleteForeignImports typesToDelete
  where
    typesToDelete =
      [CInt,CDouble,StdString,CBool] ++
      map (\(Class classType _) -> (CasadiClass classType)) classes

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


newtype FFIWrapper = FFIWrapper String
data ClassFunction = ClassFunction String String

rawName :: Class -> String
rawName c = dataName c ++ "'"

dataName :: Class -> String
dataName (Class classType _) = hsDataName classType

dataDecl :: Class -> String
dataDecl c = "newtype " ++ dataName c ++ " = " ++ dataName c ++ " (ForeignPtr " ++ rawName c ++ ")"

rawDecl :: Class -> String
rawDecl c = "data " ++ rawName c


writeClassMethods :: Class -> [(FFIWrapper, ClassFunction)]
writeClassMethods c@(Class classType methods')= methods
  where
    className = hsClassName classType

    methods = map writeMethod' methods'

    writeMethod' :: Method -> (FFIWrapper, ClassFunction)
    writeMethod' fcn = (FFIWrapper ffiWrapper, ClassFunction hsMethodName method)
      where
        method =
          unlines $
          [ hsMethodName ++ " :: " ++ typeDef
          , hsCall
          ]
        hsCall = case fMethodType fcn of
          Normal -> hsMethodName ++ " x = " ++ wrapperName ++ " (cast" ++ dataName c ++ " x)"
          _ -> hsMethodName ++ " = " ++ wrapperName
        cWrapperName'' = cWrapperName classType fcn

        hsMethodName = case fMethodType fcn of
          Constructor -> lowerCase $ prettyTics (toCName methodName)
          _ -> lowerCase (show classType) ++ "_" ++ prettyTics (toCName methodName)

        (wrapperName, ffiWrapper) = case fMethodType fcn of
          Normal -> writeFunction Nothing $ Function (Name cWrapperName'') (fType fcn) ((Ref (NonVec (CasadiClass classType))):(fArgs fcn))
          _ -> writeFunction Nothing $ Function (Name cWrapperName'') (fType fcn) (fArgs fcn)

        typeDef = case fMethodType fcn of
          Normal -> className ++ " a => " ++ concat (intersperse " -> " ("a":map (hsType False) (fArgs fcn) ++ [retType']))
          _ -> concat (intersperse " -> " (map (hsType False) (fArgs fcn) ++ [retType']))
        retType' = "IO " ++ hsType True (fType fcn)

        Name methodName = fName fcn


lowerCase :: String -> String
lowerCase [] = error "lowerCase: empty string"
lowerCase (x:xs) = toLower x : xs

typeclassName :: Class -> String
typeclassName (Class classType _ ) = hsClassName classType

typeclassDecl :: Class -> String
typeclassDecl c =
  unlines
  [ "class " ++ typeclassName c ++ " a where"
  , "  cast" ++ dataName c ++ " :: a -> " ++ dataName c
  , "instance " ++ typeclassName c ++ " " ++ dataName c ++ " where"
  , "  cast" ++ dataName c ++ " = id"
  ]

helperInstances :: Class -> String
helperInstances c =
  unlines
  [ "instance Marshal " ++ dataName c ++ " (Ptr " ++ dataName c ++ "') where"
  , "  withMarshal ("++ dataName c ++ " x) f = withForeignPtr x f"
  , "instance ForeignPtrWrapper " ++ dataName c ++ " " ++ dataName c ++ "' where"
  , "  unwrapForeignPtr ("++ dataName c ++ " x) = x"
  , "instance WrapReturn (ForeignPtr " ++ dataName c ++ "') " ++ dataName c ++ " where"
  , "  wrapReturn = return . " ++ dataName c
  ]

baseclassInstances :: Class -> [Class] -> String
baseclassInstances c bcs = unlines $ map writeInstance' bcs
  where
    writeInstance' :: Class -> String
    writeInstance' bc =
      unlines
      [ "instance " ++ typeclassName bc ++ " " ++ dataName c ++ " where"
      , "  cast" ++ dataName bc ++ " (" ++ dataName c ++ " x) = " ++ dataName bc ++ " (castForeignPtr x)"
      ]

writeFunction :: Maybe String -> Function -> (String, String)
writeFunction maybeName fcn@(Function (Name hsFunctionName') retType params) = (hsFunctionName, ffiWrapper)
  where
    ffiWrapper =
      unlines $
      [ foreignImport
      , hsFunctionName ++ "\n  :: " ++ proto
      , marshals ++ "  " ++ call
      ]
    hsFunctionName = lowerCase $ prettyTics $ fromMaybe (toCName hsFunctionName') maybeName
    (marshals, call') = marshalFun params hsFunctionName c_hsFunctionName
    call = case makesNewRef retType of
      Nothing -> call' ++ " >>= wrapReturn"
      Just v -> call' ++ " >>= (newForeignPtr " ++ c_deleteName v ++ ") >>= wrapReturn"

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

writeDeleterModule :: [Class] -> String
writeDeleterModule classes =
  init $ unlines $
  [ "{-# OPTIONS_GHC -Wall #-}"
  , "{-# Language ForeignFunctionInterface #-}"
  , ""
  , "module Casadi.Wrappers.Deleters where"
  , ""
  , "import Foreign.C.Types"
  , "import Foreign.Ptr ( FunPtr, Ptr )"
  , ""
  , "import Casadi.Wrappers.Data"
  , "import Casadi.MarshalTypes"
  , ""
  ] ++ deleters classes

writeClassModules :: [(CasadiClass, [CasadiClass])] -> [Class] -> [(String, String)]
writeClassModules _ classes = map (\x -> (dataName x,writeOneModule x)) classes
  where
    writeOneModule c =
      init $ unlines $
      [ "{-# OPTIONS_GHC -Wall #-}"
      , "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
      , "{-# Language ForeignFunctionInterface #-}"
      , "{-# Language FlexibleInstances #-}"
      , "{-# Language MultiParamTypeClasses #-}"
      , ""
      , "module Casadi.Wrappers." ++ dataName c
      , exportDecl $ [dataName c, typeclassName c] ++ sort names
      , ""
      , "import Data.Vector ( Vector )"
      , "import Foreign.C.Types"
      , "import Foreign.Ptr ( Ptr )"
      , "import Foreign.ForeignPtr ( newForeignPtr )"
      , ""
      , "import Casadi.Wrappers.ForeignToolsInstances ( )"
      , "import Casadi.Wrappers.Deleters"
      , "import Casadi.Wrappers.Data"
      , "import Casadi.MarshalTypes ( CppVec, CppVecVec, CppVecVecVec,"
      , "                             StdString', CppBool' ) -- StdOstream'"
      , "import Casadi.Marshal ( CornerCase(..), Marshal(..) )"
      , "import Casadi.WrapReturn ( WrapReturn(..) )"
      , ""
      ] ++ map f methods
      where
        methods = writeClassMethods c
        names = map (\(_, ClassFunction name _) -> name) methods
        f (FFIWrapper ffiw, ClassFunction _ cf) =
          unlines
          [ "-- direct wrapper"
          , ffiw
          , "-- classy wrapper"
          , cf
          ]

unique :: Ord a => [a] -> [a]
unique = sort . S.toList . S.fromList

writeDataModule :: [Class] -> [(CasadiClass, [CasadiClass])] -> String
writeDataModule classes inheritance =
  unlines $
  [ "{-# OPTIONS_GHC -Wall #-}"
  , "{-# Language FlexibleInstances #-}"
  , "{-# Language MultiParamTypeClasses #-}"
  , ""
  , "module Casadi.Wrappers.Data where"
  , ""
  , "import Foreign.Ptr ( Ptr )"
  , "import Foreign.ForeignPtr ( ForeignPtr, castForeignPtr, withForeignPtr )"
  , ""
  , "import Casadi.MarshalTypes ( ForeignPtrWrapper(..) )"
  , "import Casadi.Marshal (  Marshal(..) )"
  , "import Casadi.WrapReturn ( WrapReturn(..) )"
  , ""
  ] ++ map writeData classes
  where
    baseClasses :: Class -> [Class]
    baseClasses (Class classType _) = map (classMap M.!) (baseClasses' classType)

    classMap :: M.Map CasadiClass Class
    classMap = M.fromList $ map (\c@(Class cc _) -> (cc,c)) classes

    baseClasses' :: CasadiClass -> [CasadiClass]
    baseClasses' classType = case lookup classType inheritance of
      Nothing -> []
      Just xs -> unique $ xs ++ concatMap baseClasses' xs

    writeData c =
      unlines $
      [ "-- draw decl"
      , rawDecl c
      , "-- data decl"
      , dataDecl c
      , "-- typeclass decl"
      , typeclassDecl c
      , "-- baseclass instances"
      , baseclassInstances c (baseClasses c)
      , "-- helper instances"
      , helperInstances c
      ]


writeToolsModule :: [Function] -> String
writeToolsModule functions =
  unlines $
  [ "{-# OPTIONS_GHC -Wall #-}"
  , ""
  , "module Casadi.Wrappers.Tools"
  , exportDecl (sort funNames)
  , ""
  , "import Data.Vector ( Vector )"
  , "import Foreign.C.Types"
  , "import Foreign.Ptr ( Ptr )"
  , "import Foreign.ForeignPtr ( newForeignPtr )"
  , ""
  , "import Casadi.Wrappers.Data"
  , "import Casadi.Wrappers.Deleters"
  , "import Casadi.Wrappers.ForeignToolsInstances ( )"
  , "import Casadi.MarshalTypes ( CppVec, CppVecVec, CppBool', StdString' )"
  , "import Casadi.Marshal (  CornerCase(..), Marshal(..) )"
  , "import Casadi.WrapReturn ( WrapReturn(..) )"
  , ""
  ] ++ funDecls
  where
    (funNames, funDecls) = unzip $ map (\f -> writeFunction (Just (rename f)) f) functions
    rename (Function (Name uglyName) _ _) = beautifulHaskellName uglyName

    -- haskell functions can't have capital leading letter
    beautifulHaskellName :: String -> String
    beautifulHaskellName uglyName = case replaces [("_TIC","'"),("CasADi::",""),(":","_")] uglyName of
      [] -> error "beautifulHaskellName: empty string will never be beautiful :'("
      -- fall back on just making just the first letter lowercase
      "data" -> "data_"
      x -> x
