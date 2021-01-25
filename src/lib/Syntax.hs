-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- This triggers a GHC bug. Not sure that we need it anyway?
-- {-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}

module Syntax (Atom (..)) where

import qualified Data.Map.Strict as M
import Control.Exception hiding (throw)
import Control.Monad.Except hiding (Except)
import qualified Data.ByteString.Char8 as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Int
import Data.Store (Store)
import Data.Tuple (swap)
import Data.Word
import Data.Void (Void)
import Foreign.Ptr
import GHC.Generics

import HigherKinded
import Naming
import Util (IsBool (..), enumerate, (...), modifyErr)

-- === core IR ===

data Atom n =
   Var (Name n)
 | Lam Arrow (Abs EffectRow n) (LamExpr n)
 | Pi  Arrow (Abs EffectRow n)  (PiType n)
 | DataCon (Name n) (ListH Atom n) (ListH Atom n)
 | TypeCon (Name n) (ListH Atom n)
 | LabeledRow (ExtLabeledItems Type Name n)
 | Record (LabeledItems Atom n)
 | RecordTy  (ExtLabeledItems Type Name n)
 | Variant   (ExtLabeledItems Type Name n) Label Int (Atom n)
 | VariantTy (ExtLabeledItems Type Name n)
 | Con (PrimCon Atom n)
 | TC  (PrimTC  Atom n)
 | Eff (EffectRow n)
 | ACase (Atom n) (ListH (NestedAbs Type Atom) n) (Type n)
   -- single-constructor only for now
 | DataConRef (Name n) (ListH Atom n) (NestedAbs DataConRefBinding UnitH n)
 | BoxedRef (Atom n) (Block n) (AnnAbs Type Atom n)
 -- access a nested member of a binder
 -- XXX: Variable name must not be an alias for another name or for
 -- a statically-known atom. This is because the variable name used
 -- here may also appear in the type of the atom. (We maintain this
 -- invariant during substitution and in Builder.hs.)
 | ProjectElt (NE.NonEmpty Int) (Name n)
   deriving (Show, Generic)

data Expr n =
   App (Atom n) (Atom n)
 | Case (Atom n) (ListH Alt n) (Type n)
 | Atom (Atom n)
 | Op  (PrimOp  Atom n)
 | Hof (PrimHof Atom n)
   deriving (Show, Generic)

data Decl n = Let (Type n) (LetAnn, InlineHint) (Expr n)
              deriving (Show, Generic)

type Alt = NestedAbs Type Block
type LamExpr = AnnAbs Type Block
type PiType  = AnnAbs Type Type

type BlockSummary = Type  -- TODO: add effects too?
data Block n = Block (BlockSummary n) (NestedAbs Decl Atom n)
               deriving (Show, Generic)

data Module n = Module IRVariant (NestedAbs Binding SourceNameSubst n)

data Binding n =
   LetBinding (Type n) (LetAnn, InlineHint) (Expr n) -- TODO: add effect information
 | DataConDef (DataConType n) (Name n) Int   -- type, type con name, con index
 | TypeConDef (ListH Type n) (ListH Name n)  -- param types, data con names
 | LamBound   (Type n) Arrow
 | PiBound       (Type n)
 | UnknownBinder (Type n)
   deriving (Show, Generic)

type DataConType = NaryAbs (NestedAbs Type UnitH)

numDataConArgs :: DataConType n -> Int
numDataConArgs = undefined

binderType :: Binding n -> Type n
binderType = undefined

type Type = Atom
type Kind = Type
type Val = Atom Void

data Arrow = ImplicitArrow | ClassArrow | TabArrow | LinArrow
             deriving (Show, Eq, Generic)

data InlineHint = NoHint | CanInline | NoInline deriving (Show, Eq, Generic)

data LetAnn = PlainLet | InstanceLet | SuperclassLet | NoInlineLet
              deriving (Show, Eq, Generic)

data DataConRefBinding n = DataConRefBinding (Type n) (Abs Atom n)
                           deriving (Show, Generic)

data IRVariant = Surface | Typed | Core | Simp | Evaluated
                 deriving (Show, Eq, Ord, Generic)

-- === front-end language AST ===

type UExpr = WithSrc UExpr'
data UExpr' n =
   UVar (Name n)
 | ULam Arrow                   (UPatAnnAbs UExpr n)
 | UPi  Arrow (Abs EffectRow n) (UPatAnnAbs UType n)
 | UApp Arrow (UExpr n) (UExpr n)
 | UBlock (WithUDecls UExpr n)
 | UFor Direction (UPatAnnAbs UExpr n)
 | UCase (UExpr n) (ListH UAlt n)
 | UHole
 | UTypeAnn (UExpr n) (UExpr n)
 | UTabCon (ListH UExpr n)
 | UIndexRange (Limit UExpr n) (Limit UExpr n)
 | UPrimExpr (PrimExpr UExpr n)
 | URecord (ExtLabeledItems UExpr UExpr n)     -- {a=x, b=y, ...rest}
 | UVariant (LabeledItems UnitH n) Label (UExpr n)        -- {|a|b| a=x |}
 | UVariantLift (LabeledItems UnitH n) (UExpr n)          -- {|a|b| ...rest |}
 | URecordTy (ExtLabeledItems UExpr UExpr n)   -- {a:X & b:Y & ...rest}
 | UVariantTy (ExtLabeledItems UExpr UExpr n)  -- {a:X | b:Y | ...rest}
 | UIntLit  Int
 | UFloatLit Double
   deriving (Show, Generic)

type UType  = UExpr
data UConDef n = UConDef (MaybeH Name n) (NestedAbs UType UnitH n)
                 deriving (Show, Generic)
data UDecl n =
   ULet LetAnn (UPat n) (MaybeH UType n) (UExpr n)
 -- | UData UConDef [UConDef]
 -- | UInterface [UType] UConDef [UAnnBinder] -- superclasses, constructor, methods
 -- | UInstance (Maybe (Name n)) (UNest UPatAnnArrow) UType [UMethodDef]  -- name, args, type, methods
   deriving (Show, Generic)

data WithUDecls e n =
    UnsafeMakeWithDecl (UDecl n) (WithUDecls e n)
  | UDeclsResult (e n)
    deriving (Show, Generic)

data UMethodDef n = UMethodDef (Name n) (UExpr n) deriving (Show, Generic)
type UAlt = UPatAbs UExpr
data UModule n = UModule (WithUDecls SourceNameSubst n)
                 deriving (Show, Generic)

type UPat = WithSrc UPat'
data UPat' n =
   UPatIgnore
 | UPatBinder (Name n)
 -- -- | UPatCon (Name n) (UNest UPat)
 -- | UPatPair (UPat n) (UPat n)
 -- | UPatUnit
 -- | UPatRecord (ExtLabeledItems UPat UPat)              -- {a=x, b=y, ...rest}
 -- | UPatVariant     (LabeledItems () n) Label (UPat n)  -- {|a|b| a=x |}
 -- | UPatVariantLift (LabeledItems () n)       (UPat n)  -- {|a|b| ...rest |}
 -- | UPatTable [UPat n]
   deriving (Show, Generic)

type UPatAnnAbs e = PairH (MaybeH UType) (UPatAbs e)
data UPatAbs e n = UnsafeMakeUPatAbs (UPat n) (e n)
                   deriving (Show, Generic)

data NestedUPatAbs b e n =
   UnsafeMakeNestedUPatAbs (b n) (UPat n) (NestedUPatAbs b e n)
 | NestedUPatAbsResult (e n)

data WithSrc e n = WithSrc { srcPos     :: SrcCtx
                           , withoutSrc :: e n}
                   deriving (Show, Functor, Foldable)


-- === primitive constructors and operators ===

data PrimExpr e n =
        TCExpr  (PrimTC  e n)
      | ConExpr (PrimCon e n)
      | OpExpr  (PrimOp  e n)
      | HofExpr (PrimHof e n)
        deriving (Show, Generic)

data PrimTC e n =
   BaseType BaseType
 | IntRange (e n) (e n)
 | IndexRange (e n) (Limit e n) (Limit e n)
 -- Sliced index set, slice length. Note that this is no longer an index set!
 | IndexSlice (e n) (e n)
 | PairType (e n) (e n)
 | UnitType
 | RefType (MaybeH e n) (e n)
 | TypeKind
 | DataConKind
 | TypeConKind
 | EffectRowKind
 | LabeledRowKindTC
 | ParIndexRange (e n) (e n) (e n)  -- Full index set, global thread id, thread count
   deriving (Show, Generic)

data PrimCon e n =
        Lit LitVal
      | PairCon (e n) (e n)
      | UnitCon
      | ClassDictHole SrcCtx (e n)   -- Only used during type inference
      -- type, tag, payload (only used during Imp lowering)
      | SumAsProd (e n) (e n) (ListH (ListH e) n)
      -- These are just newtype wrappers. TODO: use ADTs instead
      | IntRangeVal   (e n) (e n) (e n)
      | IndexRangeVal (e n) (Limit e n) (Limit e n) (e n)
      | IndexSliceVal (e n) (e n) (e n)    -- Sliced index set, slice length, ordinal index
      | BaseTypeRef (e n)
      | TabRef (e n)
      | ConRef (PrimCon e n)
      | RecordRef (LabeledItems e n)
      | ParIndexCon (e n) (e n)        -- Type, value
        deriving (Show, Generic)

data PrimOp e n =
        TabCon (e n) (ListH e n)                 -- table type elements
      | ScalarBinOp BinOp (e n) (e n)
      | ScalarUnOp UnOp (e n)
      | Select (e n) (e n) (e n)                 -- predicate, val-if-true, val-if-false
      | PrimEffect (e n) (PrimEffect e n)
      | IndexRef (e n) (e n)
      | FstRef (e n)
      | SndRef (e n)
      | FFICall String (e n) (ListH e n)
      | Inject (e n)
      | SliceOffset (e n) (e n)              -- Index slice first, inner index second
      | SliceCurry  (e n) (e n)              -- Index slice first, curried index second
      -- Low-level memory operations
      | IOAlloc BaseType (e n)
      | IOFree (e n)
      | PtrOffset (e n) (e n)
      | PtrLoad (e n)
      | PtrStore (e n) (e n)
      -- SIMD operations
      | VectorBinOp BinOp (e n) (e n)
      | VectorPack (ListH e n)               -- List should have exactly vectorWidth elements
      | VectorIndex (e n) (e n)              -- Vector first, index second
      -- Idx (survives simplification, because we allow it to be backend-dependent)
      | UnsafeFromOrdinal (e n) (e n)   -- index set, ordinal index. XXX: doesn't check bounds
      | ToOrdinal (e n)
      | IdxSetSize (e n)
      | ThrowError (e n)
      | ThrowException (e n)             -- Catchable exceptions (unlike `ThrowError`)
      | CastOp (e n) (e n)                   -- Type, then value. See Type.hs for valid coercions.
      -- Extensible record and variant operations:
      -- Add fields to a record (on the left). Left arg contains values to add.
      | RecordCons   (LabeledItems e n) (e n)
      -- Split {a:A & b:B & ...rest} into (effectively) {a:A & b:B} & {&...rest}.
      -- Left arg contains the types of the fields to extract (e.g. a:A, b:B).
      | RecordSplit  (LabeledItems e n) (e n)
      -- Extend a variant with empty alternatives (on the left).
      -- Left arg contains the types of the empty alternatives to add.
      | VariantLift  (LabeledItems e n) (e n)
      -- Split {a:A | b:B | ...rest} into (effectively) {a:A | b:B} | {|...rest}.
      -- Left arg contains the types of the fields to extract (e.g. a:A, b:B).
      -- (see https://github.com/google-research/dex-lang/pull/201#discussion_r471591972)
      | VariantSplit (LabeledItems e n) (e n)
      -- Ask which constructor was used, as its Word8 index
      | DataConTag (e n)
      -- Create an enum (payload-free ADT) from a Word8
      | ToEnum (e n) (e n)
        deriving (Show, Generic)

data PrimHof e n =
        For ForAnn (e n)
      | Tile Int (e n) (e n)          -- dimension number, tiled body, scalar body
      | While (e n)
      | RunReader (e n) (e n)
      | RunWriter (BaseMonoidP e n) (e n)
      | RunState  (e n) (e n)
      | RunIO (e n)
      | CatchException (e n)
      | Linearize (e n)
      | Transpose (e n)
      -- accumulator monoids, index set, thread body
      | PTileReduce [BaseMonoidP e n] (e n) (e n)
        deriving (Show, Generic)

data BaseMonoidP e n = BaseMonoid { baseEmpty :: e n, baseCombine :: e n}
                       deriving (Show, Generic)
type BaseMonoid = BaseMonoidP Atom

data PrimEffect e n = MAsk | MExtend (e n) | MGet | MPut (e n)
                      deriving (Show, Generic)

data BinOp = IAdd | ISub | IMul | IDiv | ICmp CmpOp
           | FAdd | FSub | FMul | FDiv | FCmp CmpOp | FPow
           | BAnd | BOr | BShL | BShR | IRem
             deriving (Show, Eq, Generic)

data UnOp = Exp | Exp2
          | Log | Log2 | Log10 | Log1p
          | Sin | Cos | Tan | Sqrt
          | Floor | Ceil | Round
          | LGamma
          | FNeg | BNot
            deriving (Show, Eq, Generic)

data CmpOp = Less | Greater | Equal | LessEqual | GreaterEqual
             deriving (Show, Eq, Generic)

data Direction = Fwd | Rev  deriving (Show, Eq, Generic)
data ForAnn = RegularFor Direction | ParallelFor
                deriving (Show, Eq, Generic)

data Limit e n = InclusiveLim (e n)
               | ExclusiveLim (e n)
               | Unlimited
                 deriving (Show, Generic)

type PrimName = PrimExpr UnitH SourceNS

type IndexStructure = NestedAbs Type UnitH

-- === effects ===

data EffectRow n = EffectRow (S.Set (Effect n)) (Maybe (Name n))
                   deriving (Show, Eq, Generic)

data RWS = Reader | Writer | State  deriving (Show, Eq, Ord, Generic)
data Effect n = RWSEffect RWS (Name n) | ExceptionEffect | IOEffect
                deriving (Show, Eq, Ord, Generic)

pattern Pure :: EffectRow n
pattern Pure <- ((\(EffectRow effs t) -> (S.null effs, t)) -> (True, Nothing))
 where  Pure = mempty

instance Semigroup (EffectRow n) where
  EffectRow effs t <> EffectRow effs' t' =
    EffectRow (S.union effs effs') newTail
    where
      newTail = case (t, t') of
        (Nothing, effTail) -> effTail
        (effTail, Nothing) -> effTail
        _ | t == t' -> t
          | otherwise -> error "Can't combine effect rows with mismatched tails"

instance Monoid (EffectRow n) where
  mempty = EffectRow mempty Nothing

-- === labeled items ===

-- The label for a field in a record or variant.
type Label = String

-- Collection of labeled values of type `a`. Each value has a field label, and
-- multiple values can share the same label. This is the canonical form for
-- the item types in record and variant types as well as for the values in
-- record objects; the order in the concrete syntax of items with different
-- fields is discarded (so both `{b:Z & a:X & a:Y}` and `{a:X & b:Z & a:Y}` map
-- to `M.fromList [("a", NE.fromList [X, Y]), ("b", NE.fromList [Z])]` )
newtype LabeledItems e n = LabeledItems (M.Map Label (NE.NonEmpty (e n)))
                           deriving (Eq, Show, Generic)

labeledSingleton :: Label -> e n -> LabeledItems e n
labeledSingleton label value = LabeledItems $ M.singleton label (value NE.:|[])

reflectLabels :: LabeledItems e n -> LabeledItems (LiftH (Label, Int)) n
reflectLabels = undefined
-- reflectLabels (LabeledItems items) = LabeledItems $
--   flip M.mapWithKey items \k xs -> fmap (\(i,_) -> (k,i)) (enumerate xs)

getLabels :: LabeledItems e n -> [Label]
getLabels = undefined
-- getLabels labeledItems = map fst $ toListH $ reflectLabels labeledItems

withLabels :: LabeledItems e n -> LabeledItems (PairH (LiftH (Label, Int)) e) n
withLabels (LabeledItems items) = undefined
-- withLabels (LabeledItems items) = LabeledItems $
--   flip M.mapWithKey items \k xs -> fmap (\(i,a) -> (k,i,a)) (enumerate xs)

lookupLabelHead :: LabeledItems e n -> Label -> Maybe (e n)
lookupLabelHead (LabeledItems items) l = case M.lookup l items of
  Nothing -> Nothing
  Just (x NE.:| _) -> Just x

instance Semigroup (LabeledItems e n) where
  LabeledItems items <> LabeledItems items' =
    LabeledItems $ M.unionWith (<>) items items'

instance Monoid (LabeledItems e n) where
  mempty = NoLabeledItems

-- Extensible version of LabeledItems, which allows an optional object in tail
-- position. The items of the tail object will always be interpreted as a
-- "suffix" in the sense that for any field label, the object represented by
-- an ExtLabeledItems contains first the values in the (LabeledItems a) for that
-- field, followed by the values in the (Maybe b) for that field if they exist.
data ExtLabeledItems a b n = Ext (LabeledItems a n) (MaybeH b n)
  deriving (Eq, Show, Generic)

-- Adds more items to the front of an ExtLabeledItems.
prefixExtLabeledItems :: LabeledItems a n -> ExtLabeledItems a b n -> ExtLabeledItems a b n
prefixExtLabeledItems items (Ext items' rest) = Ext (items <> items') rest

-- === top-level constructs ===

data SourceBlock = SourceBlock
  { sbLine     :: Int
  , sbOffset   :: Int
  , sbLogLevel :: LogLevel
  , sbText     :: String
  , sbContents :: SourceBlock' }  deriving (Show)

type ReachedEOF = Bool
type ModuleName = String
data SourceBlock' = RunModule (UModule SourceNS)
                  | Command CmdName (SourceName, (UModule SourceNS))
                  | GetNameType SourceName
                  | ImportModule ModuleName
                  | ProseBlock String
                  | CommentLine
                  | EmptyLines
                  | UnParseable ReachedEOF String
                    deriving (Show, Generic)

data CmdName = GetType | EvalExpr OutFormat | ExportFun String
               deriving  (Show, Generic)

data LogLevel = LogNothing | PrintEvalTime | PrintBench String
              | LogPasses [PassName] | LogAll
                deriving  (Show, Generic)

-- === imperative IR ===

data IExpr n =
   ILit LitVal
 | IVar (Name n)
   deriving (Show)

type Size = IExpr
type IType = BaseType

type IFunVar = (Name (), IFunType)
data IFunType = IFunType CallingConvention [IType] [IType] -- args, results
                deriving (Show)

data IsCUDARequired = CUDARequired | CUDANotRequired  deriving (Eq, Show)

instance IsBool IsCUDARequired where
  toBool CUDARequired = True
  toBool CUDANotRequired = False

data CallingConvention = CEntryFun
                       | EntryFun IsCUDARequired
                       | FFIFun
                       | FFIMultiResultFun
                       | CUDAKernelLaunch
                       | MCThreadLaunch
                         deriving (Show)

data ImpFunction n = ImpFunction [IType] (NaryAbs ImpBlock n) deriving (Show)
data ImpBlock n = ImpBlock (NestedNaryAbs ImpDecl IExpr n)  deriving (Show)
data ImpDecl  n = ImpLet [IType] (ImpInstr n)       deriving (Show)
data ImpInstr n =
   IFor Direction (Size n) (Abs ImpBlock n)
 | IWhile (ImpBlock n)
 | ICond (IExpr n) (ImpBlock n) (ImpBlock n)
 | IQueryParallelism IFunVar (IExpr n) -- returns the number of available concurrent threads
 | ISyncWorkgroup
 | ILaunch (Name n) (Size n) (ListH IExpr n)
 | ICall (Name n) [ListH IExpr n]
 | Store (IExpr n) (IExpr n)           -- dest, val
 | Alloc AddressSpace IType (Size n)
 | MemCopy (IExpr n) (IExpr n) (IExpr n)   -- dest, source, numel
 | Free (IExpr n)
 | IThrowError  -- TODO: parameterize by a run-time string
 | ICastOp IType (IExpr n)
 | IPrimOp (PrimOp IExpr n)
   deriving (Show)

data Backend = LLVM | LLVMCUDA | LLVMMC | Interpreter  deriving (Show, Eq)
newtype CUDAKernel = CUDAKernel B.ByteString deriving (Show)

-- === base types ===

data LitVal = Int64Lit   Int64
            | Int32Lit   Int32
            | Word8Lit   Word8
            | Float64Lit Double
            | Float32Lit Float
            | PtrLit PtrType (Ptr ())
            | VecLit [LitVal]  -- Only one level of nesting allowed!
              deriving (Show, Eq, Ord, Generic)

data ScalarBaseType = Int64Type | Int32Type | Word8Type
                    | Float64Type | Float32Type
                      deriving (Show, Eq, Ord, Generic)
data BaseType = Scalar  ScalarBaseType
              | Vector  ScalarBaseType
              | PtrType PtrType
                deriving (Show, Eq, Ord, Generic)

data Device = CPU | GPU  deriving (Show, Eq, Ord, Generic)
data AddressSpace = Stack | Heap Device     deriving (Show, Eq, Ord, Generic)
type PtrType = (AddressSpace, BaseType)

sizeOf :: BaseType -> Int
sizeOf t = case t of
  Scalar Int64Type   -> 8
  Scalar Int32Type   -> 4
  Scalar Word8Type   -> 1
  Scalar Float64Type -> 8
  Scalar Float32Type -> 4
  PtrType _          -> ptrSize
  Vector st          -> vectorWidth * sizeOf (Scalar st)

ptrSize :: Int
ptrSize = 8

vectorWidth :: Int
vectorWidth = 4

-- === some handy monoids ===

data SetVal a = Set a | NotSet
newtype MonMap k v = MonMap (M.Map k v)  deriving (Show, Eq)

instance Semigroup (SetVal a) where
  x <> NotSet = x
  _ <> Set x  = Set x

instance Monoid (SetVal a) where
  mempty = NotSet

instance (Ord k, Semigroup v) => Semigroup (MonMap k v) where
  MonMap m <> MonMap m' = MonMap $ M.unionWith (<>) m m'

instance (Ord k, Semigroup v) => Monoid (MonMap k v) where
  mempty = MonMap mempty

monMapSingle :: k -> v -> MonMap k v
monMapSingle k v = MonMap (M.singleton k v)

monMapLookup :: (Monoid v, Ord k) => MonMap k v -> k -> v
monMapLookup (MonMap m) k = case M.lookup k m of Nothing -> mempty
                                                 Just v  -> v

-- === passes ===

data PassName = Parse | TypePass | SynthPass | SimpPass | ImpPass | JitPass
              | LLVMOpt | AsmPass | JAXPass | JAXSimpPass | LLVMEval
              | ResultPass | JaxprAndHLO | OptimPass
                deriving (Ord, Eq, Bounded, Enum)

instance Show PassName where
  show p = case p of
    Parse    -> "parse" ; TypePass -> "typed"   ; SynthPass -> "synth"
    SimpPass -> "simp"  ; ImpPass  -> "imp"     ; JitPass   -> "llvm"
    LLVMOpt  -> "llvmopt" ; AsmPass   -> "asm"
    JAXPass  -> "jax"   ; JAXSimpPass -> "jsimp"; ResultPass -> "result"
    LLVMEval -> "llvmeval" ; JaxprAndHLO -> "jaxprhlo"; OptimPass -> "optimized"

-- === outputs ===

type LitProg = [(SourceBlock, Result)]
type SrcCtx = Maybe SrcPos
type SrcPos = (Int, Int)
data Result = Result [Output] (Except ())  deriving (Show, Generic)

type BenchStats = (Int, Double) -- number of runs, total benchmarking time
data Output = TextOut String
            | HtmlOut String
            | PassInfo PassName String
            | EvalTime  Double (Maybe BenchStats)
            | TotalTime Double
            | BenchResult String Double Double (Maybe BenchStats) -- name, compile time, eval time
            | MiscLog String
            | ExportedFun String (Atom ())
              deriving (Show, Generic)

data OutFormat = Printed | RenderHtml  deriving (Show, Generic)

data Err = Err ErrType SrcCtx String  deriving (Show, Generic)
instance Exception Err

data ErrType = NoErr
             | ParseErr
             | TypeErr
             | KindErr
             | LinErr
             | UnboundVarErr
             | RepeatedVarErr
             | CompilerErr
             | IRVariantErr
             | NotImplementedErr
             | DataIOErr
             | MiscErr
             | RuntimeErr
               deriving Show

type Except = Either Err

throw :: MonadError Err m => ErrType -> String -> m a
throw e s = throwError $ Err e Nothing s

throwIf :: MonadError Err m => Bool -> ErrType -> String -> m ()
throwIf True  e s = throw e s
throwIf False _ _ = return ()

addContext :: MonadError Err m => String -> m a -> m a
addContext s m = modifyErr m \(Err e p s') -> Err e p (s' ++ "\n" ++ s)

addSrcContext :: MonadError Err m => SrcCtx -> m a -> m a
addSrcContext ctx m = modifyErr m updateErr
  where
    updateErr :: Err -> Err
    updateErr (Err e ctx' s) = case ctx' of Nothing -> Err e ctx  s
                                            Just _  -> Err e ctx' s

catchIOExcept :: (MonadIO m , MonadError Err m) => IO a -> m a
catchIOExcept m = (liftIO >=> liftEither) $ (liftM Right m) `catches`
  [ Handler \(e::Err)           -> return $ Left e
  , Handler \(e::IOError)       -> return $ Left $ Err DataIOErr   Nothing $ show e
  , Handler \(e::SomeException) -> return $ Left $ Err CompilerErr Nothing $ show e
  ]

instance MonadFail (Either Err) where
  fail s = Left $ Err CompilerErr Nothing s

-- -- === Synonyms ===

-- infixr 1 :-->
-- infixr 1 :--@
-- infixr 2 :==>

-- pattern (:-->) :: Type n -> Type n -> Type n
-- pattern (:-->) a b = Pi (NoAbs UnitH PureArrow) (NoAbs a b)

-- pattern (:--@) :: Type n -> Type n -> Type n
-- pattern (:--@) a b = Pi (NoAbs UnitH LinArrow) (NoAbs a b)

-- pattern (:==>) :: Type n -> Type n -> Type n
-- pattern (:==>) a b = Pi (NoAbs UnitH TabArrow) (NoAbs a b)

-- pattern IntLitExpr :: Int -> UExpr'
-- pattern IntLitExpr x = UIntLit x

-- pattern FloatLitExpr :: Double -> UExpr'
-- pattern FloatLitExpr x = UFloatLit x

-- getIntLit :: LitVal -> Int
-- getIntLit l = case l of
--   Int64Lit i -> fromIntegral i
--   Int32Lit i -> fromIntegral i
--   Word8Lit  i -> fromIntegral i
--   _ -> error $ "Expected an integer literal"

-- getFloatLit :: LitVal -> Double
-- getFloatLit l = case l of
--   Float64Lit f -> f
--   Float32Lit f -> realToFrac f
--   _ -> error $ "Expected a floating-point literal"

-- -- Type used to represent indices at run-time
-- pattern IdxRepTy :: Type n
-- pattern IdxRepTy = TC (BaseType IIdxRepTy)

-- pattern IdxRepVal :: Int32 -> Atom n
-- pattern IdxRepVal x = Con (Lit (Int32Lit x))

-- pattern IIdxRepVal :: Int32 -> IExpr
-- pattern IIdxRepVal x = ILit (Int32Lit x)

-- pattern IIdxRepTy :: IType
-- pattern IIdxRepTy = Scalar Int32Type

-- -- Type used to represent sum type tags at run-time
-- pattern TagRepTy :: Type n
-- pattern TagRepTy = TC (BaseType (Scalar Word8Type))

-- pattern TagRepVal :: Word8 -> Atom n
-- pattern TagRepVal x = Con (Lit (Word8Lit x))

-- pattern Word8Ty :: Type n
-- pattern Word8Ty = TC (BaseType (Scalar Word8Type))

-- pattern PairVal :: Atom n -> Atom n -> Atom n
-- pattern PairVal x y = Con (PairCon x y)

-- pattern PairTy :: Type n -> Type n -> Type n
-- pattern PairTy x y = TC (PairType x y)

-- pattern UnitVal :: Atom n
-- pattern UnitVal = Con UnitCon

-- pattern UnitTy :: Type n
-- pattern UnitTy = TC UnitType

-- pattern BaseTy :: BaseType -> Type n
-- pattern BaseTy b = TC (BaseType b)

-- pattern PtrTy :: PtrType -> Type n
-- pattern PtrTy ty = BaseTy (PtrType ty)

-- pattern RefTy :: Atom n -> Type n -> Type n
-- pattern RefTy r a = TC (RefType (Just r) a)

-- pattern RawRefTy :: Type n -> Type n
-- pattern RawRefTy a = TC (RefType Nothing a)

-- pattern TyKind :: Kind n
-- pattern TyKind = TC TypeKind

-- pattern EffKind :: Kind n
-- pattern EffKind = TC EffectRowKind

-- pattern LabeledRowKind :: Kind n
-- pattern LabeledRowKind = TC LabeledRowKindTC

-- pattern FixedIntRange :: Int32 -> Int32 -> Type n
-- pattern FixedIntRange low high = TC (IntRange (IdxRepVal low) (IdxRepVal high))

-- pattern Fin :: Atom n -> Type n
-- pattern Fin n = TC (IntRange (IdxRepVal 0) n)

-- -- pattern PureArrow :: Arrow n
-- -- pattern PureArrow = PlainArrow Pure

-- -- pattern TabTy :: Abs Type Type n -> Type n
-- -- pattern TabTy abs = Pi (NoAbs UnitH TabArrow) abs

-- -- XXX: These patterns should all be marked "unsafe" because they use
-- -- `UnsafeAbs`. But we'll leave that for another change, where we also expose
-- -- safer versions.

-- -- type Binder n = (Maybe (Name n), Type n)

-- -- pattern Abs :: Binder n -> body n -> Abs Type body n
-- -- pattern Abs b body <- ((\(UnsafeMakeAbs ty v body) -> ((v,ty), body)) -> (b, body))
-- --   where Abs (v,ty) body = UnsafeMakeAbs ty v body

-- -- pattern LamVal :: Binder n -> Block n -> Atom n
-- -- pattern LamVal v b <- Lam _ (Abs v b)

-- -- pattern TabVal :: Binder n -> Block n -> Atom n
-- -- pattern TabVal v b = Lam (NoAbs UnitH TabArrow) (Abs v b)

-- -- pattern TabValA :: Binder n -> Atom n -> Atom n
-- -- pattern TabValA v a = Lam (NoAbs TabArrow) (Abs v (Block (NAbsResult a)))

-- -- pattern FunTy :: Binder n -> EffectRow n -> Type n -> Type n
-- -- pattern FunTy b eff bodyTy = Pi (Abs b (PairH (PlainArrow eff) bodyTy))

-- -- pattern PiTy :: Binder n -> Arrow n -> Type n -> Type n
-- -- pattern PiTy b arr bodyTy = Pi (Abs b (PairH arr bodyTy))

-- -- pattern BinaryFunTy :: Binder n -> Binder n -> EffectRow n -> Type n -> Type n
-- -- pattern BinaryFunTy b1 b2 eff bodyTy = FunTy b1 Pure (FunTy b2 eff bodyTy)

-- -- pattern BinaryFunVal :: Binder n -> Binder n -> EffectRow n -> Block n -> Type n
-- -- pattern BinaryFunVal b1 b2 eff body =
-- --           Lam (Abs b1 (PairH PureArrow (Block (NAbsResult (
-- --           Lam (Abs b2 (PairH (PlainArrow eff) body)))))))

-- isTabTy :: Type n -> Bool
-- isTabTy = undefined
-- -- isTabTy (TabTy _ _) = True
-- -- isTabTy _ = False

-- mkConsListTy :: [Type n] -> Type n
-- mkConsListTy = foldr PairTy UnitTy

-- mkConsList :: [Atom n] -> Atom n
-- mkConsList = foldr PairVal UnitVal

-- fromConsListTy :: ( MonadError Err m) => Type n -> m [Type n]
-- fromConsListTy ty = case ty of
--   UnitTy         -> return []
--   PairTy t rest -> (t:) <$> fromConsListTy rest
--   _              -> throw CompilerErr $ "Not a pair or unit: " ++ show ty

-- -- ((...((ans & x{n}) & x{n-1})... & x2) & x1) -> (ans, [x1, ..., x{n}])
-- fromLeftLeaningConsListTy :: MonadError Err m
--                           => Int -> Type n -> m (Type n, [Type n])
-- fromLeftLeaningConsListTy depth initTy = go depth initTy []
--   where
--     go 0        ty xs = return (ty, reverse xs)
--     go remDepth ty xs = case ty of
--       PairTy lt rt -> go (remDepth - 1) lt (rt : xs)
--       _ -> throw CompilerErr $ "Not a pair: " ++ show xs

-- fromConsList :: MonadError Err m => Atom n -> m [Atom n]
-- fromConsList xs = case xs of
--   UnitVal        -> return []
--   PairVal x rest -> (x:) <$> fromConsList rest
--   _              -> throw CompilerErr $ "Not a pair or unit: " ++ show xs

pattern NoLabeledItems :: LabeledItems e n
pattern NoLabeledItems <- ((\(LabeledItems items) -> M.null items) -> True)
  where NoLabeledItems = LabeledItems M.empty

-- pattern NoExt :: LabeledItems a -> ExtLabeledItems a b
-- pattern NoExt a = Ext a Nothing

-- -- An internal label that we can use to treat records and variants as unlabeled
-- -- internal sum and product types. Note that this is not a valid label in the
-- -- concrete syntax and will be rejected by the parser (although there wouldn't
-- -- be any serious problems with overloading a user-written label).
-- pattern InternalSingletonLabel :: Label
-- pattern InternalSingletonLabel = "%UNLABELED%"

-- _getUnlabeled :: LabeledItems a -> Maybe [a]
-- _getUnlabeled (LabeledItems items) = case length items of
--   0 -> Just []
--   1 -> NE.toList <$> M.lookup InternalSingletonLabel items
--   _ -> Nothing

-- pattern Unlabeled :: [a] -> LabeledItems a
-- pattern Unlabeled as <- (_getUnlabeled -> Just as)
--   where Unlabeled as = case NE.nonEmpty as of
--           Just ne -> LabeledItems (M.singleton InternalSingletonLabel ne)
--           Nothing -> NoLabeledItems

-- -- maybeTypeDef :: TypeDef n
-- -- maybeTypeDef = TypeDef (GlobalName "Maybe") (Nest (Just "a":>TyKind) Empty)
-- --   [ DataConDef (GlobalName "Nothing") Empty
-- --   , DataConDef (GlobalName "Just"   ) (Nest (Ignore (Var (Occ "a" TyKind))) Empty)]

-- -- pattern MaybeTy :: Type n -> Type n
-- -- pattern MaybeTy a = TypeCon MaybeTypeDef [a]

-- -- pattern MaybeTypeDef :: TypeDef n
-- -- pattern MaybeTypeDef <- ((\def -> def == maybeTypeDef) -> True)
-- --   where MaybeTypeDef = maybeTypeDef

-- -- pattern NothingAtom :: Type n -> Atom n
-- -- pattern NothingAtom ty = DataCon MaybeTypeDef [ty] 0 []

-- -- pattern JustAtom :: Type n -> Atom n -> Atom n
-- -- pattern JustAtom ty x = DataCon MaybeTypeDef [ty] 1 [x]

-- -- pattern Nest :: e n -> Nest e n -> Nest e n
-- -- pattern Nest b body = NAbs (Abs b body)

-- -- pattern Empty :: Nest e n
-- -- pattern Empty = NAbsResult UnitH

-- -- pattern NestOne :: e n -> Nest e n
-- -- pattern NestOne x = NAbs (Abs x (NAbsResult UnitH))

-- -- pattern NewTypeCon :: Name n -> Type n -> [DataConDef n]
-- -- pattern NewTypeCon con ty = [DataConDef con (IgnoredNAbs [ty] UnitH)]

-- -- pattern ClassDictDef :: Name n
-- --                      -> LabeledItems (Type n) -> LabeledItems (Type n) -> [DataConDef n]
-- -- pattern ClassDictDef conName superclasses methods =
-- --   [DataConDef conName (IgnoredNAbs [ RecordTy (NoExt superclasses)
-- --                                    , RecordTy (NoExt methods)     ] UnitH)]

-- -- pattern ClassDictCon :: TypeDef n -> ListH Type n
-- --                      -> LabeledItems (Atom n) -> LabeledItems (Atom n) -> (Atom n)
-- -- pattern ClassDictCon def params superclasses methods =
-- --   DataCon def params 0 (ListH [Record superclasses, Record methods])

-- -- TODO: Enable once https://gitlab.haskell.org//ghc/ghc/issues/13363 is fixed...
-- -- {-# COMPLETE TypeVar, ArrowType, TabTy, Forall, TypeAlias, Effect, NoAnn, TC #-}

-- -- === built-in names ===

strToPrimName :: String -> Maybe PrimName
strToPrimName s = M.lookup s builtinNames

primNameToStr :: PrimName -> String
primNameToStr = undefined
-- primNameToStr prim = case lookup prim $ map swap $ M.toList builtinNames of
--   Just s  -> s
--   Nothing -> show prim

showPrimName :: PrimExpr e n -> String
showPrimName = undefined
-- showPrimName prim = primNameToStr $ fmapH (const UnitH) prim

-- TODO: Can we derive these generically? Or use Show/Read?
--       (These prelude-only names don't have to be pretty.)
builtinNames :: M.Map String PrimName
builtinNames = M.fromList
  [ ("iadd", binOp IAdd), ("isub", binOp ISub)
  , ("imul", binOp IMul), ("fdiv", binOp FDiv)
  , ("fadd", binOp FAdd), ("fsub", binOp FSub)
  , ("fmul", binOp FMul), ("idiv", binOp IDiv)
  , ("irem", binOp IRem)
  , ("fpow", binOp FPow)
  , ("and" , binOp BAnd), ("or"  , binOp BOr ), ("not" , unOp BNot)
  , ("shl" , binOp BShL), ("shr" , binOp BShR)
  , ("ieq" , binOp (ICmp Equal  )), ("feq", binOp (FCmp Equal  ))
  , ("igt" , binOp (ICmp Greater)), ("fgt", binOp (FCmp Greater))
  , ("ilt" , binOp (ICmp Less)),    ("flt", binOp (FCmp Less))
  , ("fneg", unOp  FNeg)
  , ("exp" , unOp  Exp), ("exp2"  , unOp  Exp2)
  , ("log" , unOp Log), ("log2" , unOp Log2 ), ("log10" , unOp Log10)
  , ("sin" , unOp  Sin), ("cos" , unOp Cos)
  , ("tan" , unOp  Tan), ("sqrt", unOp Sqrt)
  , ("floor", unOp Floor), ("ceil", unOp Ceil), ("round", unOp Round)
  , ("log1p", unOp Log1p), ("lgamma", unOp LGamma)
  , ("vfadd", vbinOp FAdd), ("vfsub", vbinOp FSub), ("vfmul", vbinOp FMul)
  , ("idxSetSize"  , OpExpr $ IdxSetSize UnitH)
  , ("unsafeFromOrdinal", OpExpr $ UnsafeFromOrdinal UnitH UnitH)
  , ("toOrdinal"        , OpExpr $ ToOrdinal UnitH)
  , ("throwError"     , OpExpr $ ThrowError UnitH)
  , ("throwException" , OpExpr $ ThrowException UnitH)
  , ("ask"        , OpExpr $ PrimEffect UnitH $ MAsk)
  , ("mextend"    , OpExpr $ PrimEffect UnitH $ MExtend UnitH)
  , ("get"        , OpExpr $ PrimEffect UnitH $ MGet)
  , ("put"        , OpExpr $ PrimEffect UnitH $ MPut  UnitH)
  , ("indexRef"   , OpExpr $ IndexRef UnitH UnitH)
  , ("inject"     , OpExpr $ Inject UnitH)
  , ("select"     , OpExpr $ Select UnitH UnitH UnitH)
  , ("while"           , HofExpr $ While UnitH)
  , ("linearize"       , HofExpr $ Linearize UnitH)
  , ("linearTranspose" , HofExpr $ Transpose UnitH)
  , ("runReader"       , HofExpr $ RunReader UnitH UnitH)
  , ("runWriter"       , HofExpr $ RunWriter (BaseMonoid UnitH UnitH) UnitH)
  , ("runState"        , HofExpr $ RunState  UnitH UnitH)
  , ("runIO"           , HofExpr $ RunIO UnitH)
  , ("catchException"  , HofExpr $ CatchException UnitH)
  , ("tiled"           , HofExpr $ Tile 0 UnitH UnitH)
  , ("tiledd"          , HofExpr $ Tile 1 UnitH UnitH)
  , ("TyKind"  , TCExpr $ TypeKind)
  , ("Float64" , TCExpr $ BaseType $ Scalar Float64Type)
  , ("Float32" , TCExpr $ BaseType $ Scalar Float32Type)
  , ("Int64"   , TCExpr $ BaseType $ Scalar Int64Type)
  , ("Int32"   , TCExpr $ BaseType $ Scalar Int32Type)
  , ("Word8"   , TCExpr $ BaseType $ Scalar Word8Type)
  , ("Int32Ptr", TCExpr $ BaseType $ ptrTy $ Scalar Int32Type)
  , ("Word8Ptr", TCExpr $ BaseType $ ptrTy $ Scalar Word8Type)
  , ("PtrPtr"  , TCExpr $ BaseType $ ptrTy $ ptrTy $ Scalar Word8Type)
  , ("IntRange", TCExpr $ IntRange UnitH UnitH)
  , ("Ref"     , TCExpr $ RefType (MaybeH $ Just UnitH) UnitH)
  , ("PairType", TCExpr $ PairType UnitH UnitH)
  , ("UnitType", TCExpr $ UnitType)
  , ("EffKind" , TCExpr $ EffectRowKind)
  , ("LabeledRowKind", TCExpr $ LabeledRowKindTC)
  , ("IndexSlice", TCExpr $ IndexSlice UnitH UnitH)
  , ("pair", ConExpr $ PairCon UnitH UnitH)
  , ("fstRef", OpExpr $ FstRef UnitH)
  , ("sndRef", OpExpr $ SndRef UnitH)
  -- TODO: Lift vectors to constructors
  --, ("VectorFloatType",  TCExpr $ BaseType $ Vector FloatType)
  , ("vectorPack", OpExpr $ VectorPack $ ListH $ replicate vectorWidth UnitH)
  , ("vectorIndex", OpExpr $ VectorIndex UnitH UnitH)
  , ("cast", OpExpr  $ CastOp UnitH UnitH)
  , ("sliceOffset", OpExpr $ SliceOffset UnitH UnitH)
  , ("sliceCurry", OpExpr $ SliceCurry UnitH UnitH)
  , ("alloc", OpExpr $ IOAlloc (Scalar Word8Type) UnitH)
  , ("free" , OpExpr $ IOFree UnitH)
  , ("ptrOffset", OpExpr $ PtrOffset UnitH UnitH)
  , ("ptrLoad"  , OpExpr $ PtrLoad UnitH)
  , ("ptrStore" , OpExpr $ PtrStore UnitH UnitH)
  , ("dataConTag", OpExpr $ DataConTag UnitH)
  , ("toEnum"    , OpExpr $ ToEnum UnitH UnitH)
  ]
  where
    vbinOp op = OpExpr $ VectorBinOp op UnitH UnitH
    binOp  op = OpExpr $ ScalarBinOp op UnitH UnitH
    unOp   op = OpExpr $ ScalarUnOp  op UnitH
    ptrTy  ty = PtrType (Heap CPU, ty)

-- === instances ===

-- -- TODO: make `StoreH`, a higher-kinded version of `Store`

-- instance Store a => Store (PrimOp  a)
-- instance Store a => Store (PrimCon a)
-- instance Store a => Store (PrimTC  a)
-- instance Store a => Store (PrimHof a)
-- instance Store a => Store (Limit a)
-- instance Store a => Store (PrimEffect a)
-- instance Store a => Store (BaseMonoidP a)
-- instance Store a => Store (LabeledItems a)
-- instance (Store a, Store b) => Store (ExtLabeledItems a b)
-- instance Store Arrow
-- instance Store ForAnn
-- instance Store n => Store (Atom n)
-- instance Store n => Store (Expr n)
-- instance Store n => Store (Block n)
-- instance Store n => Store (EffectRow n)
-- instance Store n => Store (Effect n)
-- instance Store RWS
-- instance Store Direction
-- instance Store UnOp
-- instance Store BinOp
-- instance Store CmpOp
-- instance Store LetAnn
-- instance Store LitVal
-- instance Store ScalarBaseType
-- instance Store BaseType
-- instance Store AddressSpace
-- instance Store Device
-- instance Store n => Store (DataConRefBinding n)
-- instance Store InlineHint

-- instance Store n => Store (Expr n)
-- instance AlphaEq  Expr
-- instance AlphaEq  Decl
-- instance Store n => Store (Decl n)

-- instance HasNames Expr
-- instance HasNames Decl
-- instance HasNames Block

-- instance Store n => Store (Block n)
-- instance AlphaEq  Block

