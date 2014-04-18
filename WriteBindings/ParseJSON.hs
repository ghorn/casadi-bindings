{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveFunctor #-}
{-# Language FlexibleInstances #-}

module WriteBindings.ParseJSON
       ( Class(..)
       , ClassType(..)
       , Namespace(..)
       , Methods
       , Method(..)
       , CppFunctions
       , CppFunction(..)
       , Name(..)
       , Type(..)
       , MethodKind(..)
       , Doc(..)
       , DocLink(..)
       , Tree(..)
       , Enum'(..)
       , EnumEntry(..)
       , Module(..)
       , readModules
       ) where

import GHC.Generics ( Generic )
import Control.Applicative ( Applicative(..) )
import Control.Arrow ( first )
import Data.Aeson
import Data.Aeson.Types ( typeMismatch )
import Data.List ( sort )
import Data.Maybe ( catMaybes )
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import qualified Data.Map.Strict as M
import Text.Parsec
--import Debug.Trace
import System.FilePath
import System.Directory ( getDirectoryContents )
import qualified Data.Set as S

instance FromJSON ClassType where
  parseJSON (String txt) =
    case parse (typeParser (const False) <* eof) "" str of
      Right x@(UserType {})    -> return (ClassType x)
      Right x@(IOSchemeVec {}) -> return (ClassType x)
      Right x@(StdVec _)       -> return (ClassType x)
      Right x@(StdPair {})     -> return (ClassType x)
      Right x@(IOInterface {}) -> return (ClassType x)
      Right y -> fail $ "\nParsec parser returned wrong class type: " ++ str ++ "\n" ++ show y
      Left x -> fail $ "\nParsec parser failed on " ++ str ++ "\n" ++ show x
    where
      str = Text.unpack txt
  parseJSON v = typeMismatch "Type" v

newtype ClassType = ClassType { unClassType :: Type } deriving (Eq, Ord, Show)
instance GetTypes ClassType where
  getTypes (ClassType t) = [t]

parseType :: IsEnum -> String -> Type
parseType isEnum str = case parse ((typeParser isEnum) <* eof) "" str of
    Right x ->
      --("parsed \"" ++ str ++ "\" -> " ++ show x) `trace`
      x
    Left x -> error $ "\nParsec parser failed on " ++ str ++ "\n" ++ show x

p :: String -> Type -> Parsec String u Type
p name tp = try $ do
  _ <- string name
  return tp

ps :: [String] -> Type -> Parsec String u Type
ps names tp = choice $ map (flip p tp) names

templatep :: IsEnum -> String -> (Type -> Type) -> Parsec String u Type
templatep isEnum name f =
  try $ fmap f $
  between (string (name++"<(")) (string ")>") (typeParser isEnum)

classp :: IsEnum -> Parsec String u Type
classp isEnum = try $ do
  ns <- namespace
  blah <- many (alphaNum <|> char '_')
  -- if there is no namespace set it to casadi::
  let ns' = case ns of Namespace [] -> Namespace ["casadi"]
                       ns'' -> ns''
  return $
    if isEnum blah
    then CEnum ns (Name blah)
    else UserType ns' (Name blah)

namespace :: Parsec String u Namespace
namespace = do
  ns <- optionMaybe namespace'
  case ns of Nothing -> return (Namespace [])
             Just xs -> return (Namespace xs)

namespace' :: Parsec String u [String]
namespace' = try $ do
    name <- manyTill (alphaNum <|> char '_') $ (try (string "::"))
    moar <- optionMaybe namespace'
    case moar of Nothing -> return [name]
                 Just xs -> return ([name]++xs)


pairp :: IsEnum -> Parsec String u Type
pairp isEnum =
  try $
  between (string "std::pair<(") (string ")>") $ do
    l <- typeParser isEnum
    _ <- char ','
    r <- typeParser isEnum
    return (StdPair l r)

ioschemevecp :: IsEnum -> Parsec String u Type
ioschemevecp isEnum =
  try $ do
    ns <- namespace
    name <- manyTill alphaNum $ (try (string "IOSchemeVector"))
    inside <- between (string "<(") (string ")>") (typeParser isEnum)
    return (IOSchemeVec ns name inside)

type IsEnum = String -> Bool

constp :: IsEnum -> Parsec String u Type
constp isEnum = try $ do
  _ <- string "q(const)."
  fmap Const (typeParser isEnum)

refp :: IsEnum -> Parsec String u Type
refp isEnum = try $ do
  _ <- string "r."
  fmap Ref (typeParser isEnum)

pointerp :: IsEnum -> Parsec String u Type
pointerp isEnum = try $ do
  _ <- string "p."
  fmap Pointer (typeParser isEnum)

arrayp :: IsEnum -> Parsec String u Type
arrayp isEnum = try $ do
  _ <- string "a()."
  fmap CArray (typeParser isEnum)

typeParser :: IsEnum -> Parsec String u Type
typeParser isEnum = do
  x <- choice
       [ p "bool" CBool
       , p "int" CInt
       , p "double" CDouble
       , p "long" CLong
       , p "void" CVoid
       , p "unsigned char" CUChar
       , p "char" CChar
       , p "std::string" StdString
       , p "std::size_t" CSize
       , p "std::ostream" StdOstream
         -- some literals
       , ps [ "casadi::Matrix<(double)>"
            ,         "Matrix<(double)>"
            ]
         (UserType (Namespace ["casadi"]) (Name "DMatrix"))

       , ps [ "casadi::Matrix<(casadi::SXElement)>"
            ,         "Matrix<(casadi::SXElement)>"
            ]
         (UserType (Namespace ["casadi"]) (Name "SX"))

       , ps [ "casadi::Matrix<(int)>"
            ,         "Matrix<(int)>"
            ]
         (UserType (Namespace ["casadi"]) (Name "IMatrix"))

         
       , constp isEnum
       , refp isEnum
       , pointerp isEnum
       , arrayp isEnum
       , templatep isEnum "std::vector" StdVec
       , templatep isEnum "casadi::GenericExpression" id
       , templatep isEnum "casadi::GenericMatrix" id
       , templatep isEnum "casadi::IOInterface" IOInterface
       , pairp isEnum
       , ioschemevecp isEnum
       , classp isEnum
       ]
  return x

newtype Doc = Doc String deriving (Generic, Eq, Ord, Show)
instance FromJSON Doc

newtype DocLink = DocLink String deriving (Generic, Eq, Ord, Show)
instance FromJSON DocLink

data MethodKind = Constructor | Static | Normal deriving (Generic, Eq, Ord, Show)
instance FromJSON MethodKind

newtype Name = Name String deriving (Generic, Show, Eq, Ord)
instance FromJSON Name
unName :: Name -> String
unName (Name x) = x

newtype Namespace = Namespace [String] deriving (Show, Eq, Ord)

data Type = CInt
          | CDouble
          | CBool
          | CVoid
          | CSize
          | CLong
          | CUChar
          | CChar
          | CArray Type
          | StdOstream
          | StdString
          | UserType Namespace Name
          | CEnum Namespace Name
          | StdPair Type Type
          | Ref Type
          | Pointer Type
          | Const Type
          | StdVec Type
          | IOSchemeVec Namespace String Type
          | IOInterface Type
          deriving (Eq, Ord, Show)

class GetTypes a where
  getTypes :: a -> [Type]

data Class' a =
  Class'
  { classType :: ClassType
  , classMethods :: [Method' a]
  , classDocs :: Doc
  , classDocslink :: DocLink
  } deriving (Generic, Eq, Ord, Functor, Show)

data Class =
  Class
  { clType :: ClassType
  , clMethods :: [Methods]
  , clDocs :: Doc
  , clDocslink :: DocLink
  } deriving (Generic, Eq, Ord, Show)

classUnion :: Class' Type -> Class' Type -> Class' Type
classUnion x y
  | ctx /= cty = error $ "classUnion: got different types\n" ++ show (ctx, cty)
  | classDocslink x /= classDocslink y = error $ "classUnion: got different docslink\n"
                                         ++ show (ctx, cty)
  | otherwise = ret
  where
    ret = x { classMethods = classMethods x ++ classMethods y
            , classDocs = docs
            , classDocslink = docslink
            }
    docs
      | classDocs x == classDocs y = classDocs x
      | otherwise = Doc ""
    docslink
      | classDocslink x == classDocslink y = classDocslink x
      | otherwise = DocLink ""

    ctx = classType x
    cty = classType y

instance GetTypes (Class' Type) where
  getTypes c = unClassType (classType c) : concatMap getTypes (classMethods c)

data Method' a =
  Method'
  { methodName :: Name
  , methodReturn :: a
  , methodParams :: [a]
  , methodKind :: MethodKind
  , methodDocs :: Doc
  , methodDocslink :: DocLink
  } deriving (Generic, Eq, Ord, Functor, Show)
instance GetTypes (Method' Type) where
  getTypes m = methodReturn m : methodParams m

data Method =
  Method
  { mName :: Name
  , mOthers :: Maybe Int
  , mReturn :: Type
  , mParams :: [Type]
  , mKind :: MethodKind
  , mDocs :: Doc
  , mDocslink :: DocLink
  } deriving (Generic, Eq, Ord, Show)
type Methods = Either [Method] Method

data CppFunction' a =
  CppFunction'
  { funName :: String
  , funReturn :: a
  , funParams :: [a]
  , funDocs :: Doc
  , funDocslink :: DocLink
  } deriving (Generic, Eq, Ord, Functor, Show)
instance GetTypes (CppFunction' Type) where
  getTypes f = funReturn f : funParams f

data CppFunction =
  CppFunction
  { fName :: String
  , fOthers :: Maybe Int
  , fReturn :: Type
  , fParams :: [Type]
  , fDocs :: Doc
  , fDocslink :: DocLink
  } deriving (Generic, Eq, Ord, Show)

data Enum' =
  Enum'
  { enumDocs :: Doc
  , enumDocslink :: DocLink
  , enumEntries :: M.Map String EnumEntry
  } deriving (Generic, Eq, Ord, Show)

data EnumEntry =
  EnumEntry
  { enumEntryDocs :: Doc
  , enumEntryDocslink :: DocLink
  , enumEntryVal :: Int
  } deriving (Generic, Eq, Ord, Show)

data Tree a =
  Tree
  { treeFunctions :: [CppFunction' a]
  , treeClasses :: [Class' a]
  , treeEnums :: M.Map String Enum'
  , treeInheritance :: M.Map String [a]
  , treeIncludes :: [String]
  } deriving (Generic, Functor, Show)

data Module =
  Module
  { moduleFunctions :: [CppFunctions]
  , moduleClasses :: M.Map ClassType Class
  , moduleEnums :: M.Map Name Enum'
  , moduleInheritance :: M.Map ClassType (S.Set ClassType)
  , moduleIncludes :: [String]
  , moduleName :: String
  }

instance FromJSON a => FromJSON (CppFunction' a)
instance FromJSON Enum'
instance FromJSON EnumEntry
instance FromJSON a => FromJSON (Tree a)
instance FromJSON a => FromJSON (Method' a)
instance FromJSON a => FromJSON (Class' a)

uncasadi :: String -> String
uncasadi xs
  | take 7 xs == "casadi_" = drop 7 xs
  | otherwise = error $ "uncasadi: " ++ xs

getModulePaths :: FilePath -> FilePath -> Maybe (String, FilePath)
getModulePaths rootpath name
  | reverse (take 5 (reverse name)) == ".json" = Just (uncasadi modname, treepath)
  | otherwise = Nothing
  where
    modname = reverse $ drop 5 $ reverse name
    treepath = combine rootpath (modname ++ ".json")

parseModule :: (String, FilePath) -> IO (String, Tree String)
parseModule (modname, treepath) = do
  putStrLn $ "parsing " ++ modname
  tree <- fmap eitherDecode' (BS.readFile treepath) :: IO (Either String (Tree String))
  tparsed <- case tree of
    Left err -> error $ "module parsing failure: " ++ err
    Right ret -> return ret
  return (modname, tparsed)

getAllTypes :: Type -> [Type]
getAllTypes x@(CArray t) = x:getAllTypes t
getAllTypes x@(StdPair tx ty) = x : concatMap getAllTypes [tx,ty]
getAllTypes x@(Ref t) = x : getAllTypes t
getAllTypes x@(Pointer t) = x : getAllTypes t
getAllTypes x@(Const t) = x : getAllTypes t
getAllTypes x@(StdVec t) = x : getAllTypes t
getAllTypes x@(IOSchemeVec _ _ t) = x : getAllTypes t
getAllTypes x@(IOInterface t) = x : getAllTypes t
getAllTypes x = [x]

hasBadType :: GetTypes a => a -> Bool
hasBadType x = any badType $ concatMap getAllTypes (getTypes x)
  where
    badType (IOSchemeVec {}) = True
    badType (IOInterface (UserType (Namespace ["casadi"]) (Name "Function"))) = False
    badType (IOInterface {}) = True
    badType StdOstream = True
    badType (StdPair {}) = True
    badType (Pointer {}) = True
    badType (CArray {}) = True
    badType (UserType _ (Name name))
      | name `elem` ["Dictionary"] = True
    badType _ = False

startsWith :: String -> String -> Bool
startsWith x y = take n x == y
  where
    n = length y

hasBadMethodName :: String -> Bool
hasBadMethodName xs = any (xs `startsWith`) ["operator "]

filterMethods :: Class' Type -> Class' Type
filterMethods c = c { classMethods =
                         filter (not . hasBadMethodName . unName . methodName)
                         $ filter (not . hasBadType) (classMethods c)
                    }

overloadMethods :: Class' Type -> Class
overloadMethods c =
  Class
  { clType = classType c
  , clMethods = map m2ms (M.toList ms)
  , clDocs = classDocs c
  , clDocslink = classDocslink c
  }
  where
    ms :: M.Map Name [Method' Type]
    ms = M.fromListWith (++) $ map (\m -> (methodName m, [m])) (classMethods c)

    m2ms :: (Name, [Method' Type]) -> Methods
    m2ms (Name name, []) = error $ "method '" ++ name ++ "' has 0 instances"
    m2ms (_, [f]) = Right (m2m f Nothing)
    m2ms (_, fs) = Left $ zipWith (\f k -> m2m f (Just k)) fs [0..]

    m2m :: Method' Type -> Maybe Int -> Method
    m2m f others =
      Method
      { mName = methodName f
      , mOthers = others
      , mKind = methodKind f
      , mReturn = methodReturn f
      , mParams = methodParams f
      , mDocs = methodDocs f
      , mDocslink = methodDocslink f
      }

type CppFunctions = Either [CppFunction] CppFunction

readModules :: FilePath -> IO [Module]
readModules rootpath = do
  contents <- getDirectoryContents rootpath
  let mods = sort $ catMaybes $ map (getModulePaths rootpath) contents
  trees0 <- mapM parseModule mods :: IO [(String, Tree String)]

  let isEnum :: String -> Bool
      isEnum = (`S.member` (S.fromList (concatMap (M.keys . treeEnums . snd) trees0)))

      trees = map (\(x,y) -> (x, fmap (parseType isEnum) y)) trees0

      treeToModule :: (String, Tree Type) -> Module
      treeToModule (modname, tree) =
        Module
        { moduleName = modname
        , moduleFunctions = map f2fs (M.toList functions0) :: [CppFunctions]
        , moduleClasses = M.map (overloadMethods . filterMethods)
                          $ M.fromListWith classUnion
                          $ filter (not . hasBadType . fst)
                          $ map (\c -> (classType c, c)) $ treeClasses tree
        , moduleEnums        = M.fromList $ map (first Name) $ M.toList $ treeEnums tree
        , moduleIncludes     = treeIncludes tree
        , moduleInheritance  = newInheritance
        }
        where
          newInheritance =
            M.map (S.filter (not . hasBadType) . S.fromList)
            $ M.fromListWith (++)
            $ filter (not . hasBadType . fst)
            $ map (\(k,v) -> (ClassType (parseType isEnum k), map ClassType v))
            $ M.toList (treeInheritance tree)

          functions0 :: M.Map String [CppFunction' Type]
          functions0 =
            M.map (filter (not . hasBadType) . filter (not . hasBadMethodName . funName))
            $ M.fromListWith (++) $ map (\x -> (funName x, [x])) $ treeFunctions tree

          f2fs :: (String, [CppFunction' Type]) -> CppFunctions
          f2fs (name, []) = error $ "function '" ++ name ++ "' has 0 instances"
          f2fs (_, [f]) = Right (f2f f Nothing)
          f2fs (_, fs) = Left $ zipWith (\f k -> f2f f (Just k)) fs [0..]

          f2f :: CppFunction' Type -> Maybe Int -> CppFunction
          f2f f others =
            CppFunction
            { fName = funName f
            , fOthers = others
            , fReturn = funReturn f
            , fParams = funParams f
            , fDocs = funDocs f
            , fDocslink = funDocslink f
            }

      modules = map treeToModule trees

--      pmi :: M.Map ClassType (S.Set ClassType) -> IO ()
--      pmi xs = do
--        putStrLn "=============================================================================="
--        let pmi' (ct, inh) = do
--              putStrLn ""
--              print ct
--              mapM_ (putStrLn . ("      " ++) . show) (S.toList inh)
--        mapM_ pmi' (M.toList xs)
--  mapM_ (pmi . moduleInheritance) modules

  return modules


main :: IO ()
main = do
  let rootpath = "/home/ghorn/casadi/build/swig"
  _ <- readModules rootpath
  return ()
