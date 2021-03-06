{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Data where

import Prelude hiding ( Functor )

import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.ForeignPtr ( ForeignPtr, castForeignPtr, newForeignPtr, touchForeignPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )

import Casadi.Internal.Marshal (  Marshal(..) )
import Casadi.Internal.WrapReturn ( WrapReturn(..) )
-- raw decl
data Callback'
-- data decl
{-|
-}
newtype Callback = Callback (ForeignPtr Callback')
-- typeclass decl
class CallbackClass a where
  castCallback :: a -> Callback
instance CallbackClass Callback where
  castCallback = id

-- baseclass instances
instance FunctionClass Callback where
  castFunction (Callback x) = Function (castForeignPtr x)

instance PrintableCommonClass Callback where
  castPrintableCommon (Callback x) = PrintableCommon (castForeignPtr x)

instance SharedObjectClass Callback where
  castSharedObject (Callback x) = SharedObject (castForeignPtr x)


-- helper instances
instance Marshal Callback (Ptr Callback') where
  marshal (Callback x) = return (unsafeForeignPtrToPtr x)
  marshalFree (Callback x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__Callback" 
  c_delete_casadi__Callback :: FunPtr (Ptr Callback' -> IO ())
instance WrapReturn (Ptr Callback') Callback where
  wrapReturn = (fmap Callback) . (newForeignPtr c_delete_casadi__Callback)


-- raw decl
data CasadiMeta'
-- data decl
{-|
-}
newtype CasadiMeta = CasadiMeta (ForeignPtr CasadiMeta')
-- typeclass decl
class CasadiMetaClass a where
  castCasadiMeta :: a -> CasadiMeta
instance CasadiMetaClass CasadiMeta where
  castCasadiMeta = id

-- baseclass instances

-- helper instances
instance Marshal CasadiMeta (Ptr CasadiMeta') where
  marshal (CasadiMeta x) = return (unsafeForeignPtrToPtr x)
  marshalFree (CasadiMeta x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__CasadiMeta" 
  c_delete_casadi__CasadiMeta :: FunPtr (Ptr CasadiMeta' -> IO ())
instance WrapReturn (Ptr CasadiMeta') CasadiMeta where
  wrapReturn = (fmap CasadiMeta) . (newForeignPtr c_delete_casadi__CasadiMeta)


-- raw decl
data CodeGenerator'
-- data decl
{-|
-}
newtype CodeGenerator = CodeGenerator (ForeignPtr CodeGenerator')
-- typeclass decl
class CodeGeneratorClass a where
  castCodeGenerator :: a -> CodeGenerator
instance CodeGeneratorClass CodeGenerator where
  castCodeGenerator = id

-- baseclass instances

-- helper instances
instance Marshal CodeGenerator (Ptr CodeGenerator') where
  marshal (CodeGenerator x) = return (unsafeForeignPtrToPtr x)
  marshalFree (CodeGenerator x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__CodeGenerator" 
  c_delete_casadi__CodeGenerator :: FunPtr (Ptr CodeGenerator' -> IO ())
instance WrapReturn (Ptr CodeGenerator') CodeGenerator where
  wrapReturn = (fmap CodeGenerator) . (newForeignPtr c_delete_casadi__CodeGenerator)


-- raw decl
data DM'
-- data decl
{-|
-}
newtype DM = DM (ForeignPtr DM')
-- typeclass decl
class DMClass a where
  castDM :: a -> DM
instance DMClass DM where
  castDM = id

-- baseclass instances
instance GenericExpressionCommonClass DM where
  castGenericExpressionCommon (DM x) = GenericExpressionCommon (castForeignPtr x)

instance GenericMatrixCommonClass DM where
  castGenericMatrixCommon (DM x) = GenericMatrixCommon (castForeignPtr x)

instance MatrixCommonClass DM where
  castMatrixCommon (DM x) = MatrixCommon (castForeignPtr x)

instance PrintableCommonClass DM where
  castPrintableCommon (DM x) = PrintableCommon (castForeignPtr x)

instance SparsityInterfaceCommonClass DM where
  castSparsityInterfaceCommon (DM x) = SparsityInterfaceCommon (castForeignPtr x)


-- helper instances
instance Marshal DM (Ptr DM') where
  marshal (DM x) = return (unsafeForeignPtrToPtr x)
  marshalFree (DM x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__DM" 
  c_delete_casadi__DM :: FunPtr (Ptr DM' -> IO ())
instance WrapReturn (Ptr DM') DM where
  wrapReturn = (fmap DM) . (newForeignPtr c_delete_casadi__DM)


-- raw decl
data DaeBuilder'
-- data decl
{-|
-}
newtype DaeBuilder = DaeBuilder (ForeignPtr DaeBuilder')
-- typeclass decl
class DaeBuilderClass a where
  castDaeBuilder :: a -> DaeBuilder
instance DaeBuilderClass DaeBuilder where
  castDaeBuilder = id

-- baseclass instances
instance PrintableCommonClass DaeBuilder where
  castPrintableCommon (DaeBuilder x) = PrintableCommon (castForeignPtr x)


-- helper instances
instance Marshal DaeBuilder (Ptr DaeBuilder') where
  marshal (DaeBuilder x) = return (unsafeForeignPtrToPtr x)
  marshalFree (DaeBuilder x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__DaeBuilder" 
  c_delete_casadi__DaeBuilder :: FunPtr (Ptr DaeBuilder' -> IO ())
instance WrapReturn (Ptr DaeBuilder') DaeBuilder where
  wrapReturn = (fmap DaeBuilder) . (newForeignPtr c_delete_casadi__DaeBuilder)


-- raw decl
data Function'
-- data decl
{-|
-}
newtype Function = Function (ForeignPtr Function')
-- typeclass decl
class FunctionClass a where
  castFunction :: a -> Function
instance FunctionClass Function where
  castFunction = id

-- baseclass instances
instance PrintableCommonClass Function where
  castPrintableCommon (Function x) = PrintableCommon (castForeignPtr x)

instance SharedObjectClass Function where
  castSharedObject (Function x) = SharedObject (castForeignPtr x)


-- helper instances
instance Marshal Function (Ptr Function') where
  marshal (Function x) = return (unsafeForeignPtrToPtr x)
  marshalFree (Function x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__Function" 
  c_delete_casadi__Function :: FunPtr (Ptr Function' -> IO ())
instance WrapReturn (Ptr Function') Function where
  wrapReturn = (fmap Function) . (newForeignPtr c_delete_casadi__Function)


-- raw decl
data GenericExpressionCommon'
-- data decl
{-|
-}
newtype GenericExpressionCommon = GenericExpressionCommon (ForeignPtr GenericExpressionCommon')
-- typeclass decl
class GenericExpressionCommonClass a where
  castGenericExpressionCommon :: a -> GenericExpressionCommon
instance GenericExpressionCommonClass GenericExpressionCommon where
  castGenericExpressionCommon = id

-- baseclass instances

-- helper instances
instance Marshal GenericExpressionCommon (Ptr GenericExpressionCommon') where
  marshal (GenericExpressionCommon x) = return (unsafeForeignPtrToPtr x)
  marshalFree (GenericExpressionCommon x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__GenericExpressionCommon" 
  c_delete_casadi__GenericExpressionCommon :: FunPtr (Ptr GenericExpressionCommon' -> IO ())
instance WrapReturn (Ptr GenericExpressionCommon') GenericExpressionCommon where
  wrapReturn = (fmap GenericExpressionCommon) . (newForeignPtr c_delete_casadi__GenericExpressionCommon)


-- raw decl
data GenericMatrixCommon'
-- data decl
{-|
-}
newtype GenericMatrixCommon = GenericMatrixCommon (ForeignPtr GenericMatrixCommon')
-- typeclass decl
class GenericMatrixCommonClass a where
  castGenericMatrixCommon :: a -> GenericMatrixCommon
instance GenericMatrixCommonClass GenericMatrixCommon where
  castGenericMatrixCommon = id

-- baseclass instances

-- helper instances
instance Marshal GenericMatrixCommon (Ptr GenericMatrixCommon') where
  marshal (GenericMatrixCommon x) = return (unsafeForeignPtrToPtr x)
  marshalFree (GenericMatrixCommon x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__GenericMatrixCommon" 
  c_delete_casadi__GenericMatrixCommon :: FunPtr (Ptr GenericMatrixCommon' -> IO ())
instance WrapReturn (Ptr GenericMatrixCommon') GenericMatrixCommon where
  wrapReturn = (fmap GenericMatrixCommon) . (newForeignPtr c_delete_casadi__GenericMatrixCommon)


-- raw decl
data GenericType'
-- data decl
{-|
-}
newtype GenericType = GenericType (ForeignPtr GenericType')
-- typeclass decl
class GenericTypeClass a where
  castGenericType :: a -> GenericType
instance GenericTypeClass GenericType where
  castGenericType = id

-- baseclass instances
instance PrintableCommonClass GenericType where
  castPrintableCommon (GenericType x) = PrintableCommon (castForeignPtr x)

instance SharedObjectClass GenericType where
  castSharedObject (GenericType x) = SharedObject (castForeignPtr x)


-- helper instances
instance Marshal GenericType (Ptr GenericType') where
  marshal (GenericType x) = return (unsafeForeignPtrToPtr x)
  marshalFree (GenericType x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__GenericType" 
  c_delete_casadi__GenericType :: FunPtr (Ptr GenericType' -> IO ())
instance WrapReturn (Ptr GenericType') GenericType where
  wrapReturn = (fmap GenericType) . (newForeignPtr c_delete_casadi__GenericType)


-- raw decl
data GlobalOptions'
-- data decl
{-|
-}
newtype GlobalOptions = GlobalOptions (ForeignPtr GlobalOptions')
-- typeclass decl
class GlobalOptionsClass a where
  castGlobalOptions :: a -> GlobalOptions
instance GlobalOptionsClass GlobalOptions where
  castGlobalOptions = id

-- baseclass instances

-- helper instances
instance Marshal GlobalOptions (Ptr GlobalOptions') where
  marshal (GlobalOptions x) = return (unsafeForeignPtrToPtr x)
  marshalFree (GlobalOptions x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__GlobalOptions" 
  c_delete_casadi__GlobalOptions :: FunPtr (Ptr GlobalOptions' -> IO ())
instance WrapReturn (Ptr GlobalOptions') GlobalOptions where
  wrapReturn = (fmap GlobalOptions) . (newForeignPtr c_delete_casadi__GlobalOptions)


-- raw decl
data IM'
-- data decl
{-|
-}
newtype IM = IM (ForeignPtr IM')
-- typeclass decl
class IMClass a where
  castIM :: a -> IM
instance IMClass IM where
  castIM = id

-- baseclass instances
instance GenericExpressionCommonClass IM where
  castGenericExpressionCommon (IM x) = GenericExpressionCommon (castForeignPtr x)

instance GenericMatrixCommonClass IM where
  castGenericMatrixCommon (IM x) = GenericMatrixCommon (castForeignPtr x)

instance MatrixCommonClass IM where
  castMatrixCommon (IM x) = MatrixCommon (castForeignPtr x)

instance PrintableCommonClass IM where
  castPrintableCommon (IM x) = PrintableCommon (castForeignPtr x)

instance SparsityInterfaceCommonClass IM where
  castSparsityInterfaceCommon (IM x) = SparsityInterfaceCommon (castForeignPtr x)


-- helper instances
instance Marshal IM (Ptr IM') where
  marshal (IM x) = return (unsafeForeignPtrToPtr x)
  marshalFree (IM x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__IM" 
  c_delete_casadi__IM :: FunPtr (Ptr IM' -> IO ())
instance WrapReturn (Ptr IM') IM where
  wrapReturn = (fmap IM) . (newForeignPtr c_delete_casadi__IM)


-- raw decl
data Importer'
-- data decl
{-|
-}
newtype Importer = Importer (ForeignPtr Importer')
-- typeclass decl
class ImporterClass a where
  castImporter :: a -> Importer
instance ImporterClass Importer where
  castImporter = id

-- baseclass instances
instance PrintableCommonClass Importer where
  castPrintableCommon (Importer x) = PrintableCommon (castForeignPtr x)

instance SharedObjectClass Importer where
  castSharedObject (Importer x) = SharedObject (castForeignPtr x)


-- helper instances
instance Marshal Importer (Ptr Importer') where
  marshal (Importer x) = return (unsafeForeignPtrToPtr x)
  marshalFree (Importer x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__Importer" 
  c_delete_casadi__Importer :: FunPtr (Ptr Importer' -> IO ())
instance WrapReturn (Ptr Importer') Importer where
  wrapReturn = (fmap Importer) . (newForeignPtr c_delete_casadi__Importer)


-- raw decl
data IndexAbstraction'
-- data decl
{-|
-}
newtype IndexAbstraction = IndexAbstraction (ForeignPtr IndexAbstraction')
-- typeclass decl
class IndexAbstractionClass a where
  castIndexAbstraction :: a -> IndexAbstraction
instance IndexAbstractionClass IndexAbstraction where
  castIndexAbstraction = id

-- baseclass instances

-- helper instances
instance Marshal IndexAbstraction (Ptr IndexAbstraction') where
  marshal (IndexAbstraction x) = return (unsafeForeignPtrToPtr x)
  marshalFree (IndexAbstraction x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__IndexAbstraction" 
  c_delete_casadi__IndexAbstraction :: FunPtr (Ptr IndexAbstraction' -> IO ())
instance WrapReturn (Ptr IndexAbstraction') IndexAbstraction where
  wrapReturn = (fmap IndexAbstraction) . (newForeignPtr c_delete_casadi__IndexAbstraction)


-- raw decl
data Linsol'
-- data decl
{-|
-}
newtype Linsol = Linsol (ForeignPtr Linsol')
-- typeclass decl
class LinsolClass a where
  castLinsol :: a -> Linsol
instance LinsolClass Linsol where
  castLinsol = id

-- baseclass instances
instance PrintableCommonClass Linsol where
  castPrintableCommon (Linsol x) = PrintableCommon (castForeignPtr x)

instance SharedObjectClass Linsol where
  castSharedObject (Linsol x) = SharedObject (castForeignPtr x)


-- helper instances
instance Marshal Linsol (Ptr Linsol') where
  marshal (Linsol x) = return (unsafeForeignPtrToPtr x)
  marshalFree (Linsol x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__Linsol" 
  c_delete_casadi__Linsol :: FunPtr (Ptr Linsol' -> IO ())
instance WrapReturn (Ptr Linsol') Linsol where
  wrapReturn = (fmap Linsol) . (newForeignPtr c_delete_casadi__Linsol)


-- raw decl
data MX'
-- data decl
{-|
-}
newtype MX = MX (ForeignPtr MX')
-- typeclass decl
class MXClass a where
  castMX :: a -> MX
instance MXClass MX where
  castMX = id

-- baseclass instances
instance GenericExpressionCommonClass MX where
  castGenericExpressionCommon (MX x) = GenericExpressionCommon (castForeignPtr x)

instance GenericMatrixCommonClass MX where
  castGenericMatrixCommon (MX x) = GenericMatrixCommon (castForeignPtr x)

instance PrintableCommonClass MX where
  castPrintableCommon (MX x) = PrintableCommon (castForeignPtr x)

instance SharedObjectClass MX where
  castSharedObject (MX x) = SharedObject (castForeignPtr x)

instance SparsityInterfaceCommonClass MX where
  castSparsityInterfaceCommon (MX x) = SparsityInterfaceCommon (castForeignPtr x)


-- helper instances
instance Marshal MX (Ptr MX') where
  marshal (MX x) = return (unsafeForeignPtrToPtr x)
  marshalFree (MX x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__MX" 
  c_delete_casadi__MX :: FunPtr (Ptr MX' -> IO ())
instance WrapReturn (Ptr MX') MX where
  wrapReturn = (fmap MX) . (newForeignPtr c_delete_casadi__MX)


-- raw decl
data MatrixCommon'
-- data decl
{-|
-}
newtype MatrixCommon = MatrixCommon (ForeignPtr MatrixCommon')
-- typeclass decl
class MatrixCommonClass a where
  castMatrixCommon :: a -> MatrixCommon
instance MatrixCommonClass MatrixCommon where
  castMatrixCommon = id

-- baseclass instances

-- helper instances
instance Marshal MatrixCommon (Ptr MatrixCommon') where
  marshal (MatrixCommon x) = return (unsafeForeignPtrToPtr x)
  marshalFree (MatrixCommon x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__MatrixCommon" 
  c_delete_casadi__MatrixCommon :: FunPtr (Ptr MatrixCommon' -> IO ())
instance WrapReturn (Ptr MatrixCommon') MatrixCommon where
  wrapReturn = (fmap MatrixCommon) . (newForeignPtr c_delete_casadi__MatrixCommon)


-- raw decl
data MetaCon'
-- data decl
{-|
-}
newtype MetaCon = MetaCon (ForeignPtr MetaCon')
-- typeclass decl
class MetaConClass a where
  castMetaCon :: a -> MetaCon
instance MetaConClass MetaCon where
  castMetaCon = id

-- baseclass instances
instance IndexAbstractionClass MetaCon where
  castIndexAbstraction (MetaCon x) = IndexAbstraction (castForeignPtr x)


-- helper instances
instance Marshal MetaCon (Ptr MetaCon') where
  marshal (MetaCon x) = return (unsafeForeignPtrToPtr x)
  marshalFree (MetaCon x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__MetaCon" 
  c_delete_casadi__MetaCon :: FunPtr (Ptr MetaCon' -> IO ())
instance WrapReturn (Ptr MetaCon') MetaCon where
  wrapReturn = (fmap MetaCon) . (newForeignPtr c_delete_casadi__MetaCon)


-- raw decl
data MetaVar'
-- data decl
{-|
-}
newtype MetaVar = MetaVar (ForeignPtr MetaVar')
-- typeclass decl
class MetaVarClass a where
  castMetaVar :: a -> MetaVar
instance MetaVarClass MetaVar where
  castMetaVar = id

-- baseclass instances
instance IndexAbstractionClass MetaVar where
  castIndexAbstraction (MetaVar x) = IndexAbstraction (castForeignPtr x)


-- helper instances
instance Marshal MetaVar (Ptr MetaVar') where
  marshal (MetaVar x) = return (unsafeForeignPtrToPtr x)
  marshalFree (MetaVar x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__MetaVar" 
  c_delete_casadi__MetaVar :: FunPtr (Ptr MetaVar' -> IO ())
instance WrapReturn (Ptr MetaVar') MetaVar where
  wrapReturn = (fmap MetaVar) . (newForeignPtr c_delete_casadi__MetaVar)


-- raw decl
data NlpBuilder'
-- data decl
{-|
-}
newtype NlpBuilder = NlpBuilder (ForeignPtr NlpBuilder')
-- typeclass decl
class NlpBuilderClass a where
  castNlpBuilder :: a -> NlpBuilder
instance NlpBuilderClass NlpBuilder where
  castNlpBuilder = id

-- baseclass instances
instance PrintableCommonClass NlpBuilder where
  castPrintableCommon (NlpBuilder x) = PrintableCommon (castForeignPtr x)


-- helper instances
instance Marshal NlpBuilder (Ptr NlpBuilder') where
  marshal (NlpBuilder x) = return (unsafeForeignPtrToPtr x)
  marshalFree (NlpBuilder x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__NlpBuilder" 
  c_delete_casadi__NlpBuilder :: FunPtr (Ptr NlpBuilder' -> IO ())
instance WrapReturn (Ptr NlpBuilder') NlpBuilder where
  wrapReturn = (fmap NlpBuilder) . (newForeignPtr c_delete_casadi__NlpBuilder)


-- raw decl
data Opti'
-- data decl
{-|
-}
newtype Opti = Opti (ForeignPtr Opti')
-- typeclass decl
class OptiClass a where
  castOpti :: a -> Opti
instance OptiClass Opti where
  castOpti = id

-- baseclass instances
instance PrintableCommonClass Opti where
  castPrintableCommon (Opti x) = PrintableCommon (castForeignPtr x)

instance SharedObjectClass Opti where
  castSharedObject (Opti x) = SharedObject (castForeignPtr x)


-- helper instances
instance Marshal Opti (Ptr Opti') where
  marshal (Opti x) = return (unsafeForeignPtrToPtr x)
  marshalFree (Opti x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__Opti" 
  c_delete_casadi__Opti :: FunPtr (Ptr Opti' -> IO ())
instance WrapReturn (Ptr Opti') Opti where
  wrapReturn = (fmap Opti) . (newForeignPtr c_delete_casadi__Opti)


-- raw decl
data OptiAdvanced'
-- data decl
{-|
-}
newtype OptiAdvanced = OptiAdvanced (ForeignPtr OptiAdvanced')
-- typeclass decl
class OptiAdvancedClass a where
  castOptiAdvanced :: a -> OptiAdvanced
instance OptiAdvancedClass OptiAdvanced where
  castOptiAdvanced = id

-- baseclass instances
instance OptiClass OptiAdvanced where
  castOpti (OptiAdvanced x) = Opti (castForeignPtr x)

instance PrintableCommonClass OptiAdvanced where
  castPrintableCommon (OptiAdvanced x) = PrintableCommon (castForeignPtr x)

instance SharedObjectClass OptiAdvanced where
  castSharedObject (OptiAdvanced x) = SharedObject (castForeignPtr x)


-- helper instances
instance Marshal OptiAdvanced (Ptr OptiAdvanced') where
  marshal (OptiAdvanced x) = return (unsafeForeignPtrToPtr x)
  marshalFree (OptiAdvanced x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__OptiAdvanced" 
  c_delete_casadi__OptiAdvanced :: FunPtr (Ptr OptiAdvanced' -> IO ())
instance WrapReturn (Ptr OptiAdvanced') OptiAdvanced where
  wrapReturn = (fmap OptiAdvanced) . (newForeignPtr c_delete_casadi__OptiAdvanced)


-- raw decl
data OptiCallback'
-- data decl
{-|
-}
newtype OptiCallback = OptiCallback (ForeignPtr OptiCallback')
-- typeclass decl
class OptiCallbackClass a where
  castOptiCallback :: a -> OptiCallback
instance OptiCallbackClass OptiCallback where
  castOptiCallback = id

-- baseclass instances

-- helper instances
instance Marshal OptiCallback (Ptr OptiCallback') where
  marshal (OptiCallback x) = return (unsafeForeignPtrToPtr x)
  marshalFree (OptiCallback x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__OptiCallback" 
  c_delete_casadi__OptiCallback :: FunPtr (Ptr OptiCallback' -> IO ())
instance WrapReturn (Ptr OptiCallback') OptiCallback where
  wrapReturn = (fmap OptiCallback) . (newForeignPtr c_delete_casadi__OptiCallback)


-- raw decl
data OptiSol'
-- data decl
{-|
-}
newtype OptiSol = OptiSol (ForeignPtr OptiSol')
-- typeclass decl
class OptiSolClass a where
  castOptiSol :: a -> OptiSol
instance OptiSolClass OptiSol where
  castOptiSol = id

-- baseclass instances
instance PrintableCommonClass OptiSol where
  castPrintableCommon (OptiSol x) = PrintableCommon (castForeignPtr x)


-- helper instances
instance Marshal OptiSol (Ptr OptiSol') where
  marshal (OptiSol x) = return (unsafeForeignPtrToPtr x)
  marshalFree (OptiSol x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__OptiSol" 
  c_delete_casadi__OptiSol :: FunPtr (Ptr OptiSol' -> IO ())
instance WrapReturn (Ptr OptiSol') OptiSol where
  wrapReturn = (fmap OptiSol) . (newForeignPtr c_delete_casadi__OptiSol)


-- raw decl
data PrintableCommon'
-- data decl
{-|
-}
newtype PrintableCommon = PrintableCommon (ForeignPtr PrintableCommon')
-- typeclass decl
class PrintableCommonClass a where
  castPrintableCommon :: a -> PrintableCommon
instance PrintableCommonClass PrintableCommon where
  castPrintableCommon = id

-- baseclass instances

-- helper instances
instance Marshal PrintableCommon (Ptr PrintableCommon') where
  marshal (PrintableCommon x) = return (unsafeForeignPtrToPtr x)
  marshalFree (PrintableCommon x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__PrintableCommon" 
  c_delete_casadi__PrintableCommon :: FunPtr (Ptr PrintableCommon' -> IO ())
instance WrapReturn (Ptr PrintableCommon') PrintableCommon where
  wrapReturn = (fmap PrintableCommon) . (newForeignPtr c_delete_casadi__PrintableCommon)


-- raw decl
data SX'
-- data decl
{-|
-}
newtype SX = SX (ForeignPtr SX')
-- typeclass decl
class SXClass a where
  castSX :: a -> SX
instance SXClass SX where
  castSX = id

-- baseclass instances
instance GenericExpressionCommonClass SX where
  castGenericExpressionCommon (SX x) = GenericExpressionCommon (castForeignPtr x)

instance GenericMatrixCommonClass SX where
  castGenericMatrixCommon (SX x) = GenericMatrixCommon (castForeignPtr x)

instance MatrixCommonClass SX where
  castMatrixCommon (SX x) = MatrixCommon (castForeignPtr x)

instance PrintableCommonClass SX where
  castPrintableCommon (SX x) = PrintableCommon (castForeignPtr x)

instance SparsityInterfaceCommonClass SX where
  castSparsityInterfaceCommon (SX x) = SparsityInterfaceCommon (castForeignPtr x)


-- helper instances
instance Marshal SX (Ptr SX') where
  marshal (SX x) = return (unsafeForeignPtrToPtr x)
  marshalFree (SX x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__SX" 
  c_delete_casadi__SX :: FunPtr (Ptr SX' -> IO ())
instance WrapReturn (Ptr SX') SX where
  wrapReturn = (fmap SX) . (newForeignPtr c_delete_casadi__SX)


-- raw decl
data SXElem'
-- data decl
{-|
-}
newtype SXElem = SXElem (ForeignPtr SXElem')
-- typeclass decl
class SXElemClass a where
  castSXElem :: a -> SXElem
instance SXElemClass SXElem where
  castSXElem = id

-- baseclass instances

-- helper instances
instance Marshal SXElem (Ptr SXElem') where
  marshal (SXElem x) = return (unsafeForeignPtrToPtr x)
  marshalFree (SXElem x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__SXElem" 
  c_delete_casadi__SXElem :: FunPtr (Ptr SXElem' -> IO ())
instance WrapReturn (Ptr SXElem') SXElem where
  wrapReturn = (fmap SXElem) . (newForeignPtr c_delete_casadi__SXElem)


-- raw decl
data SharedObject'
-- data decl
{-|
-}
newtype SharedObject = SharedObject (ForeignPtr SharedObject')
-- typeclass decl
class SharedObjectClass a where
  castSharedObject :: a -> SharedObject
instance SharedObjectClass SharedObject where
  castSharedObject = id

-- baseclass instances

-- helper instances
instance Marshal SharedObject (Ptr SharedObject') where
  marshal (SharedObject x) = return (unsafeForeignPtrToPtr x)
  marshalFree (SharedObject x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__SharedObject" 
  c_delete_casadi__SharedObject :: FunPtr (Ptr SharedObject' -> IO ())
instance WrapReturn (Ptr SharedObject') SharedObject where
  wrapReturn = (fmap SharedObject) . (newForeignPtr c_delete_casadi__SharedObject)


-- raw decl
data Slice'
-- data decl
{-|
-}
newtype Slice = Slice (ForeignPtr Slice')
-- typeclass decl
class SliceClass a where
  castSlice :: a -> Slice
instance SliceClass Slice where
  castSlice = id

-- baseclass instances
instance PrintableCommonClass Slice where
  castPrintableCommon (Slice x) = PrintableCommon (castForeignPtr x)


-- helper instances
instance Marshal Slice (Ptr Slice') where
  marshal (Slice x) = return (unsafeForeignPtrToPtr x)
  marshalFree (Slice x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__Slice" 
  c_delete_casadi__Slice :: FunPtr (Ptr Slice' -> IO ())
instance WrapReturn (Ptr Slice') Slice where
  wrapReturn = (fmap Slice) . (newForeignPtr c_delete_casadi__Slice)


-- raw decl
data Sparsity'
-- data decl
{-|
-}
newtype Sparsity = Sparsity (ForeignPtr Sparsity')
-- typeclass decl
class SparsityClass a where
  castSparsity :: a -> Sparsity
instance SparsityClass Sparsity where
  castSparsity = id

-- baseclass instances
instance PrintableCommonClass Sparsity where
  castPrintableCommon (Sparsity x) = PrintableCommon (castForeignPtr x)

instance SharedObjectClass Sparsity where
  castSharedObject (Sparsity x) = SharedObject (castForeignPtr x)

instance SparsityInterfaceCommonClass Sparsity where
  castSparsityInterfaceCommon (Sparsity x) = SparsityInterfaceCommon (castForeignPtr x)


-- helper instances
instance Marshal Sparsity (Ptr Sparsity') where
  marshal (Sparsity x) = return (unsafeForeignPtrToPtr x)
  marshalFree (Sparsity x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__Sparsity" 
  c_delete_casadi__Sparsity :: FunPtr (Ptr Sparsity' -> IO ())
instance WrapReturn (Ptr Sparsity') Sparsity where
  wrapReturn = (fmap Sparsity) . (newForeignPtr c_delete_casadi__Sparsity)


-- raw decl
data SparsityInterfaceCommon'
-- data decl
{-|
-}
newtype SparsityInterfaceCommon = SparsityInterfaceCommon (ForeignPtr SparsityInterfaceCommon')
-- typeclass decl
class SparsityInterfaceCommonClass a where
  castSparsityInterfaceCommon :: a -> SparsityInterfaceCommon
instance SparsityInterfaceCommonClass SparsityInterfaceCommon where
  castSparsityInterfaceCommon = id

-- baseclass instances

-- helper instances
instance Marshal SparsityInterfaceCommon (Ptr SparsityInterfaceCommon') where
  marshal (SparsityInterfaceCommon x) = return (unsafeForeignPtrToPtr x)
  marshalFree (SparsityInterfaceCommon x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__SparsityInterfaceCommon" 
  c_delete_casadi__SparsityInterfaceCommon :: FunPtr (Ptr SparsityInterfaceCommon' -> IO ())
instance WrapReturn (Ptr SparsityInterfaceCommon') SparsityInterfaceCommon where
  wrapReturn = (fmap SparsityInterfaceCommon) . (newForeignPtr c_delete_casadi__SparsityInterfaceCommon)


-- raw decl
data Variable'
-- data decl
{-|
-}
newtype Variable = Variable (ForeignPtr Variable')
-- typeclass decl
class VariableClass a where
  castVariable :: a -> Variable
instance VariableClass Variable where
  castVariable = id

-- baseclass instances
instance PrintableCommonClass Variable where
  castPrintableCommon (Variable x) = PrintableCommon (castForeignPtr x)


-- helper instances
instance Marshal Variable (Ptr Variable') where
  marshal (Variable x) = return (unsafeForeignPtrToPtr x)
  marshalFree (Variable x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__Variable" 
  c_delete_casadi__Variable :: FunPtr (Ptr Variable' -> IO ())
instance WrapReturn (Ptr Variable') Variable where
  wrapReturn = (fmap Variable) . (newForeignPtr c_delete_casadi__Variable)


-- raw decl
data WeakRef'
-- data decl
{-|
-}
newtype WeakRef = WeakRef (ForeignPtr WeakRef')
-- typeclass decl
class WeakRefClass a where
  castWeakRef :: a -> WeakRef
instance WeakRefClass WeakRef where
  castWeakRef = id

-- baseclass instances
instance SharedObjectClass WeakRef where
  castSharedObject (WeakRef x) = SharedObject (castForeignPtr x)


-- helper instances
instance Marshal WeakRef (Ptr WeakRef') where
  marshal (WeakRef x) = return (unsafeForeignPtrToPtr x)
  marshalFree (WeakRef x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__WeakRef" 
  c_delete_casadi__WeakRef :: FunPtr (Ptr WeakRef' -> IO ())
instance WrapReturn (Ptr WeakRef') WeakRef where
  wrapReturn = (fmap WeakRef) . (newForeignPtr c_delete_casadi__WeakRef)


-- raw decl
data XmlFile'
-- data decl
{-|
-}
newtype XmlFile = XmlFile (ForeignPtr XmlFile')
-- typeclass decl
class XmlFileClass a where
  castXmlFile :: a -> XmlFile
instance XmlFileClass XmlFile where
  castXmlFile = id

-- baseclass instances
instance PrintableCommonClass XmlFile where
  castPrintableCommon (XmlFile x) = PrintableCommon (castForeignPtr x)

instance SharedObjectClass XmlFile where
  castSharedObject (XmlFile x) = SharedObject (castForeignPtr x)


-- helper instances
instance Marshal XmlFile (Ptr XmlFile') where
  marshal (XmlFile x) = return (unsafeForeignPtrToPtr x)
  marshalFree (XmlFile x) _ = touchForeignPtr x
foreign import ccall unsafe "&delete_casadi__XmlFile" 
  c_delete_casadi__XmlFile :: FunPtr (Ptr XmlFile' -> IO ())
instance WrapReturn (Ptr XmlFile') XmlFile where
  wrapReturn = (fmap XmlFile) . (newForeignPtr c_delete_casadi__XmlFile)


