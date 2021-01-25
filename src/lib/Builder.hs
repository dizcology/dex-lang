-- Copyright 2021 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Builder (emit, emitAnn, emitOp, buildDepEffLam, buildLamAux, buildPi,
                getAllowedEffects, withEffects, modifyAllowedEffects,
                buildLam, BuilderT, Builder, MonadBuilder,
                buildScoped, buildScopedBlock, runBuilderT, emitWithBindings,
                runSubstBuilder, runBuilder, getBindings, lookupBinding,
                builderLook, liftBuilder,
                app, getTypeE, add, mul, sub, neg, div',
                iadd, imul, isub, idiv, ilt, ieq,
                fpow, flog, fLitLike, recGetHead,
                select, substBuilder, substBuilderR, emitUnpack, getUnpacked,
                fromPair, getFst, getSnd, getFstRef, getSndRef,
                naryApp, appReduce, appTryReduce, buildAbs, buildNAbs,
                buildFor, buildForAux, buildForAnn, buildForAnnAux,
                emitBlock, unzipTab, isSingletonType, withNameHint,
                singletonTypeVal, builderScoped, extendBindings,
                checkBuilder, builderExtend, applyPreludeFunction,
                unpackConsList, unpackLeftLeaningConsList,
                emitRunWriter, emitRunWriters, mextendForRef,
                emitRunState, emitMaybeCase, emitWhile,
                emitRunReader, tabGet, SubstBuilderT, SubstBuilder, runSubstBuilderT,
                ptrOffset, ptrLoad, unsafePtrLoad,
                evalBlockE, substTraversalDef,
                TraversalDef, asBlock,
                traverseBlock, traverseExpr, traverseAtom,
                clampPositive, buildTypeDef,
                transformModuleAsBlock, appReduceTraversalDef,
                indexSetSizeE, indexToIntE, intToIndexE,
                extendSourceSubst, getSourceSubst, lookupSourceSubst) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except hiding (Except)
import Control.Monad.Reader
import Control.Monad.Writer hiding (Alt)
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Foldable (toList)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text.Prettyprint.Doc
import Data.Tuple (swap)
import GHC.Stack

import Env
import Syntax
import Cat
import Type
import PPrint
import Util (bindM2, scanM, restructure, singletonSL, fromSL, SnocList)

newtype BuilderT n m a = BuilderT (ReaderT (BuilderEnvR n) (CatT  (BuilderEnvC n) m) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, Alternative)

type Builder n = BuilderT n Identity
type BuilderEnv n = (BuilderEnvR n, BuilderEnvC n)

type SubstBuilderT i o m = ReaderT (Subst i o) (BuilderT o m)
type SubstBuilder  i o   = SubstBuilderT i o Identity

type BuilderEnvC n = Bindings n
data BuilderEnvR n = BuilderEnvR
  { builderNameHint    :: NameStr
  , builderAllowedEffs :: EffectRow n
  , builderSourceSubst :: SourceSubst n }

runBuilderT :: Monad m => BuilderT n m (e n) -> TopEnv n -> m (WithBindings e n)
runBuilderT (BuilderT m) bindings = undefined
-- runBuilderT (BuilderT m) bindings = do
--   (ans, env) <- runCatT (runReaderT m ("tmp", Pure)) (bindings, Empty)
--   return (ans, env)

runBuilder :: Builder n (e n) -> TopEnv n -> WithBindings e n
runBuilder m env = runIdentity $ runBuilderT m env

runSubstBuilderT :: Monad m => SubstBuilderT i o m (e o)
                 -> Subst i o -> TopEnv o -> m (WithBindings e o)
runSubstBuilderT m subst env = runBuilderT (runReaderT m subst) env

runSubstBuilder :: SubstBuilder i o (e o) -> Subst i o -> TopEnv o -> WithBindings e o
runSubstBuilder m subst env = runIdentity $ runBuilderT (runReaderT m subst) env

emit :: MonadBuilder n m => Expr n -> m (Atom n)
emit expr     = emitAnn PlainLet expr

emitAnn :: MonadBuilder n m => LetAnn -> Expr n -> m (Atom n)
emitAnn = undefined
-- emitAnn ann expr = do
--   bindings <- getBindings
--   (b, v) <- freshVarE (LetBound ann expr) $ getType expr
--   builderExtend $ asSnd $ Nest (Let ann b expr) Empty
--   return v

getTypeE :: (MonadBuilder n m, HasType e) => e n -> m (Type n)
getTypeE x = do
  typeEnv <- getBindings
  return $ getType typeEnv x

emitOp :: MonadBuilder n m => Op n -> m (Atom n)
emitOp op = emit $ Op op

emitUnpack :: MonadBuilder n m => Expr n -> m [Atom n]
emitUnpack expr = getUnpacked =<< emit expr

emitBlock :: MonadBuilder n m => Block n -> m (Atom n)
emitBlock block = undefined -- emitBlockRec mempty block

emitBlockRec :: MonadBuilder n m => Subst n n -> Block n -> m (Atom n)
emitBlockRec = undefined
-- emitBlockRec env (Block (Nest (Let ann b expr) decls) result) = do
--   expr' <- substBuilder env expr
--   x <- withNameHint b $ emitAnn ann expr'
--   emitBlockRec (env <> b@>x) $ Block decls result
-- emitBlockRec env (Block Empty (Atom atom)) = substBuilder env atom
-- emitBlockRec env (Block Empty expr) = substBuilder env expr >>= emit

freshName :: MonadBuilder n m => Binding n -> m (Name n)
freshName = undefined

freshRecNames :: MonadBuilder n m => [(Maybe NameStr, Binding n)] -> m [Name n]
freshRecNames = undefined

-- Useful for tying the knot with recursive bindings. Use with care.
updateBinding :: MonadBuilder n m => Name n -> Binding n -> m ()
updateBinding = undefined

-- freshVarE :: MonadBuilder n m => Binding n -> Type n -> m (Binder n, Atom n)
-- freshVarE bInfo ty = do
--   hint <- askNameStr
--   bindings <- getBindings
--   let v = genFresh NormalName hint bindings
--   builderExtend $ asFst $  v @> (ty, bInfo)
--   return (Just v :> ty,  Var $ Occ v ty)

-- freshNestedBinders :: MonadBuilder n m => Nest Binder n -> m (Nest Var n)
-- freshNestedBinders bs = freshNestedBindersRec mempty bs

-- freshNestedBindersRec :: MonadBuilder n m => Env n (Atom n)
--                       -> Nest Binder n -> m (Nest Var n)
-- freshNestedBindersRec = undefined
-- freshNestedBindersRec _ Empty = return Empty
-- freshNestedBindersRec substEnv (Nest b bs) = do
--   bindings <- getBindings
--   v  <- freshVarE PatBound $ subst (substEnv, bindings) b
--   vs <- freshNestedBindersRec (substEnv <> b@>Var v) bs
--   return $ Nest v vs

buildPi :: (MonadError Err m, MonadBuilder n m)
        => Type n -> (Atom n -> m (Arrow n, Type n)) -> m (Atom n)
buildPi = undefined
-- buildPi ty f = do
--   bindings <- getBindings
--   (ans, decls) <- scopedDecls $ do
--      (b, x) <- freshVarE PiBound ty
--      (arr, ans) <- f x
--      return $ Pi $ makeAbs b (WithArrow arr ans)
--   let block = wrapDecls decls ans
--   case typeReduceBlock bindings block of
--     Just piTy -> return piTy
--     Nothing -> throw CompilerErr $
--       "Unexpected irreducible decls in pi type: " ++ pprint decls

buildAbs :: MonadBuilder n m
         => Type n -> Binding n
         -> (Atom n -> m (e n))
         -> m (Abs Type (WithBindings e) n)
buildAbs = undefined
-- buildAbs ty f = do
--   ((b, ans), decls) <- scopedDecls $ do
--      (b, x) <- freshVarE UnknownBinder ty
--      ans <- f x
--      return (b, ans)
--   return (Abs b (Abs decls ans))

buildLam :: MonadBuilder n m => Type n -> Arrow n
         -> (Atom n -> m (Atom n)) -> m (Atom n)
buildLam ty arr body = buildDepEffLam ty (const (return arr)) body

buildDepEffLam :: MonadBuilder n m
               => Type n
               -> (Atom n -> m (Arrow n))
               -> (Atom n -> m (Atom n))
               -> m (Atom n)
buildDepEffLam b fArr fBody = liftM fst $ buildLamAux b fArr \x -> (,()) <$> fBody x

asBlock :: MonadBuilder n m => WithBindings Atom n -> m (Block n)
asBlock = undefined

buildLamAux :: MonadBuilder n m
            => Type n
            -> (Atom n -> m (Arrow n))
            -> (Atom n -> m (Atom n, a))
            -> m (Atom n, a)
buildLamAux = undefined
-- buildLamAux b fArr fBody = do
--   ((b', arr, ans, aux), decls) <- scopedDecls $ do
--      (b, x) <- freshVarE UnknownBinder b
--      arr <- fArr x
--      -- overwriting the previous binder info know that we know more
--      builderExtend $ asFst $ v @> (varType v, LamBound (void arr))
--      (ans, aux) <- withEffects (arrowEff arr) $ fBody x
--      return (Bind v, arr, ans, aux)
--   return (Lam $ makeAbs b' (WithArrow arr $ wrapDecls decls ans), aux)

buildNAbs :: MonadBuilder n m => NAbs Type HUnit n
          -> ([Atom n] -> m (Atom n)) -> m (NAbs Type Block n)
buildNAbs bs body = undefined -- liftM fst $ buildNAbsAux bs \xs -> (,()) <$> body xs

-- buildNAbsAux :: MonadBuilder n m => Nest Binder n
--              -> ([Atom n] -> m (Atom n, a)) -> m (Alt n, a)
-- buildNAbsAux bs body = undefined
-- buildNAbsAux bs body = do
--   ((bs', (ans, aux)), decls) <- scopedDecls $ do
--      vs <- freshNestedBinders bs
--      result <- body $ map Var $ toList vs
--      return (fmap Bind vs, result)
--   return (Abs bs' $ wrapDecls decls ans, aux)

-- buildTypeDef :: MonadBuilder n m
-- buildTypeDef = undefined


-- buildTypeDef tyConName paramBinders body = do
--   ((paramBinders', dataDefs), _) <- scopedDecls $ do
--      vs <- freshNestedBinders paramBinders
--      result <- body $ map Var $ toList vs
--      return (fmap Bind vs, result)
--   return $ TypeDef tyConName paramBinders' dataDefs

-- buildImplicitNaryLam :: MonadBuilder n m => (Nest Binder n)
--                      -> ([Atom n] -> m (Atom n)) -> m (Atom n)
-- buildImplicitNaryLam Empty body = body []
-- buildImplicitNaryLam (Nest b bs) body =
--   buildLam b ImplicitArrow \x -> do
--     bs' <- substBuilder (b@>x) bs
--     buildImplicitNaryLam bs' \xs -> body $ x:xs

buildTypeDef :: MonadBuilder n m
             => NameStr -> [NameStr] -> Type n
             -> (Atom n -> m [Type n])
             -> m (Atom n, [Atom n])
buildTypeDef tyConHint dataConHints tyConType dataConTyBuilders = undefined

recGetHead :: MonadBuilder n m => Label -> Atom n -> m (Atom n)
recGetHead l x = do
  ~(RecordTy (Ext r _)) <- getTypeE x
  let i = fromJust $ elemIndex l $ map fst $ toList $ reflectLabels r
  return $ getProjection [i] x

buildScopedBlock :: MonadBuilder n m => m (Atom n) -> m (Block n)
buildScopedBlock = undefined

buildScoped :: MonadBuilder n m => m (e n) -> m (WithBindings e n)
buildScoped = undefined

emitWithBindings :: MonadBuilder n m => WithBindings e n -> m (e n)
emitWithBindings = undefined

-- buildScoped m = do
--   (abs, ()) <- buildScopedAux $ (,()) <$> m
--   return $ Block abs

-- buildScopedAux :: MonadBuilder n m => m (e n, a) -> m (NAbs Decl e n, a)
-- buildScopedAux m = undefined

wrapDecls :: [Decl n] -> Atom n -> Block n
wrapDecls = undefined
-- wrapDecls decls atom = inlineLastDecl $ Block decls $ Atom atom

inlineLastDecl :: Block n -> Block n
inlineLastDecl = undefined
-- inlineLastDecl block@(Block decls result) =
--   case (reverse (fromNest decls), result) of
--     (Let _ (Just v :> ty) expr:rest, Atom atom) | atom == Var (Occ v ty) ->
--       Block (toNest (reverse rest)) expr
--     _ -> block

fLitLike :: MonadBuilder n m => Double -> Atom n -> m (Atom n)
fLitLike x t = do
  ty <- getTypeE t
  return case ty of
    BaseTy (Scalar Float64Type) -> Con $ Lit $ Float64Lit x
    BaseTy (Scalar Float32Type) -> Con $ Lit $ Float32Lit $ realToFrac x
    _ -> error "Expected a floating point scalar"

neg :: MonadBuilder n m => Atom n -> m (Atom n)
neg x = emitOp $ ScalarUnOp FNeg x

add :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
add x y = emitOp $ ScalarBinOp FAdd x y

-- TODO: Implement constant folding for fixed-width integer types as well!
iadd :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
iadd (Con (Lit l)) y | getIntLit l == 0 = return y
iadd x (Con (Lit l)) | getIntLit l == 0 = return x
iadd x@(Con (Lit _)) y@(Con (Lit _)) = return $ applyIntBinOp (+) x y
iadd x y = emitOp $ ScalarBinOp IAdd x y

mul :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
mul x y = emitOp $ ScalarBinOp FMul x y

imul :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
imul   (Con (Lit l)) y               | getIntLit l == 1 = return y
imul x                 (Con (Lit l)) | getIntLit l == 1 = return x
imul x@(Con (Lit _)) y@(Con (Lit _))                    = return $ applyIntBinOp (*) x y
imul x y = emitOp $ ScalarBinOp IMul x y

sub :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
sub x y = emitOp $ ScalarBinOp FSub x y

isub :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
isub x (Con (Lit l)) | getIntLit l == 0 = return x
isub x@(Con (Lit _)) y@(Con (Lit _)) = return $ applyIntBinOp (-) x y
isub x y = emitOp $ ScalarBinOp ISub x y

select :: MonadBuilder n m => Atom n -> Atom n -> Atom n -> m (Atom n)
select (Con (Lit (Word8Lit p))) x y = return $ if p /= 0 then x else y
select p x y = emitOp $ Select p x y

div' :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
div' x y = emitOp $ ScalarBinOp FDiv x y

idiv :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
idiv x (Con (Lit l)) | getIntLit l == 1 = return x
idiv x@(Con (Lit _)) y@(Con (Lit _)) = return $ applyIntBinOp div x y
idiv x y = emitOp $ ScalarBinOp IDiv x y

irem :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
irem x y = emitOp $ ScalarBinOp IRem x y

fpow :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
fpow x y = emitOp $ ScalarBinOp FPow x y

flog :: MonadBuilder n m => Atom n -> m (Atom n)
flog x = emitOp $ ScalarUnOp Log x

ilt :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
ilt x@(Con (Lit _)) y@(Con (Lit _)) = return $ applyIntCmpOp (<) x y
ilt x y = emitOp $ ScalarBinOp (ICmp Less) x y

ieq :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
ieq x@(Con (Lit _)) y@(Con (Lit _)) = return $ applyIntCmpOp (==) x y
ieq x y = emitOp $ ScalarBinOp (ICmp Equal) x y

fromPair :: MonadBuilder n m => Atom n -> m (Atom n, Atom n)
fromPair pair = do
  ~[x, y] <- getUnpacked pair
  return (x, y)

getFst :: MonadBuilder n m => Atom n -> m (Atom n)
getFst p = fst <$> fromPair p

getSnd :: MonadBuilder n m => Atom n -> m (Atom n)
getSnd p = snd <$> fromPair p

getFstRef :: MonadBuilder n m => Atom n -> m (Atom n)
getFstRef r = emitOp $ FstRef r

getSndRef :: MonadBuilder n m => Atom n -> m (Atom n)
getSndRef r = emitOp $ SndRef r

-- XXX: getUnpacked must reduce its argument to enforce the invariant that
-- ProjectElt atoms are always fully reduced (to avoid type errors between two
-- equivalent types spelled differently).
getUnpacked :: MonadBuilder n m => Atom n -> m [Atom n]
getUnpacked atom = do
  bindings <- getBindings
  len <- projectLength <$> getTypeE atom
  let atom' = typeReduceAtom bindings atom
  let res = map (\i -> getProjection [i] atom') [0..(len-1)]
  return res

app :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
app x i = emit $ App x i

naryApp :: MonadBuilder n m => Atom n -> [Atom n] -> m (Atom n)
naryApp f xs = foldM app f xs

appReduce :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
appReduce = undefined
-- appReduce (Lam _ (UnsafeMakeAbs _ v b)) a =
--   runReaderT (evalBlockE substTraversalDef b) (foldMap (@>a) v)
-- appReduce _ _ = error "appReduce expected a lambda as the first argument"

-- TODO: this would be more convenient if we could add type inference too
applyPreludeFunction :: MonadBuilder n m => String -> [Atom n] -> m (Atom n)
applyPreludeFunction = undefined
-- applyPreludeFunction s xs = do
--   bindings <- getBindings
--   case envLookup bindings fname of
--     Nothing -> error $ "Function not defined yet: " ++ s
--     Just (ty, _) -> naryApp (Var (Occ fname ty)) xs
--   where fname = SourceName $ fromString s

appTryReduce :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
appTryReduce f x = case f of
  Lam _ _ -> appReduce f x
  _       -> app f x

ptrOffset :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
ptrOffset x i = emitOp $ PtrOffset x i

unsafePtrLoad :: MonadBuilder n m => Atom n -> m (Atom n)
unsafePtrLoad x = undefined
-- unsafePtrLoad x = emit $ Hof $ RunIO $ Lam $ Abs (Ignore UnitTy) $
--   WithArrow (PlainArrow (oneEffect IOEffect)) $ Block (NAbsBody (Op (PtrLoad x)))

ptrLoad :: MonadBuilder n m => Atom n -> m (Atom n)
ptrLoad x = emitOp $ PtrLoad x

unpackConsList :: MonadBuilder n m => Atom n -> m [Atom n]
unpackConsList xs = do
  ty <- getTypeE xs
  case ty of
    UnitTy -> return []
    --PairTy _ UnitTy -> (:[]) <$> getFst xs
    PairTy _ _ -> do
      (x, rest) <- fromPair xs
      liftM (x:) $ unpackConsList rest
    _ -> error $ "Not a cons list: " ++ pprint ty

-- ((...((ans, x{n}), x{n-1})..., x2), x1) -> (ans, [x1, ..., x{n}])
-- This is useful for unpacking results of stacked effect handlers (as produced
-- by e.g. emitRunWriters).
unpackLeftLeaningConsList :: MonadBuilder n m
                          => Int -> Atom n -> m (Atom n, [Atom n])
unpackLeftLeaningConsList depth atom = go depth atom []
  where
    go 0        curAtom xs = return (curAtom, reverse xs)
    go remDepth curAtom xs = do
      (consTail, x) <- fromPair curAtom
      go (remDepth - 1) consTail (x : xs)

emitWhile :: MonadBuilder n m => m (Atom n) -> m ()
emitWhile body = do
  eff <- getAllowedEffects
  lam <- buildLam UnitTy (PlainArrow eff) \_ -> body
  void $ emit $ Hof $ While lam

emitMaybeCase :: MonadBuilder n m => Atom n -> m (Atom n)
              -> (Atom n -> m (Atom n)) -> m (Atom n)
emitMaybeCase = undefined
-- emitMaybeCase scrut nothingCase justCase = do
--   let (MaybeTy a) = getType scrut
--   nothingAlt <- buildNAbs Empty                        \[]  -> nothingCase
--   justAlt    <- buildNAbs (Nest (Just "x" :> a) Empty) \[x] -> justCase x
--   let (Abs _ nothingBody) = nothingAlt
--   let resultTy = getType nothingBody
--   emit $ Case scrut [nothingAlt, justAlt] resultTy

-- monoidLift :: Type n -> Type n -> Nest Binder n
-- monoidLift baseTy accTy = case baseTy == accTy of
--   True  -> Empty
--   False -> case accTy of
--     TabTy n b -> Nest n $ monoidLift baseTy b
--     _         -> error $ "Base monoid type mismatch: can't lift " ++
--                          pprint baseTy ++ " to " ++ pprint accTy

mextendForRef :: MonadBuilder n m => Atom n -> BaseMonoid n -> Atom n -> m (Atom n)
mextendForRef = undefined
-- mextendForRef ref (BaseMonoid _ combine) update = do
--   buildLam accTy PureArrow \refVal ->
--     buildNestedFor (repeat Fwd) (ClosedBindings liftIndices) $ \indices -> do
--       refElem <- tabGetNd refVal indices
--       updateElem <- tabGetNd update indices
--       bindM2 appTryReduce (appTryReduce combine refElem) (return updateElem)
--   where
--     TC (RefType _ accTy) = getType ref
--     FunTy (BinderAnn baseTy) _ _ = getType combine
--     liftIndices = monoidLift baseTy accTy

emitRunWriter :: MonadBuilder n m
              => Type n -> BaseMonoid n
              -> (Atom n -> m (Atom n)) -> m (Atom n)
emitRunWriter accTy bm body = do
  emit . Hof . RunWriter bm =<< mkBinaryEffFun Writer accTy body

emitRunWriters :: MonadBuilder n m
               => [(Type n, BaseMonoid n)]
               -> ([Atom n] -> m (Atom n)) -> m (Atom n)
emitRunWriters inits body = go inits []
  where
    go [] refs = body $ reverse refs
    go ((accTy, bm):rest) refs = emitRunWriter accTy bm $ \ref -> go rest (ref:refs)

emitRunReader :: MonadBuilder n m
              => Atom n -> (Atom n -> m (Atom n)) -> m (Atom n)
emitRunReader x0 body = do
  ty <- getTypeE x0
  emit . Hof . RunReader x0 =<< mkBinaryEffFun Reader ty body

emitRunState :: MonadBuilder n m
             => Atom n -> (Atom n -> m (Atom n)) -> m (Atom n)
emitRunState x0 body = do
  ty <- getTypeE x0
  emit . Hof . RunState x0 =<< mkBinaryEffFun State ty body

mkBinaryEffFun :: MonadBuilder n m
               => RWS -> Type n -> (Atom n -> m (Atom n)) -> m (Atom n)
mkBinaryEffFun rws ty body = do
  eff <- getAllowedEffects
  buildLam TyKind PureArrow \r@(Var rName) -> do
    let arr = PlainArrow $ extendEffect (RWSEffect rws rName) eff
    buildLam (RefTy r ty) arr body

buildForAnnAux :: MonadBuilder n m => ForAnn -> Type n
               -> (Atom n -> m (Atom n, a)) -> m (Atom n, a)
buildForAnnAux ann ty body = do
  -- TODO: consider only tracking the effects that are actually needed.
  eff <- getAllowedEffects
  (lam, aux) <- buildLamAux ty (const $ return $ PlainArrow eff) body
  (,aux) <$> (emit $ Hof $ For ann lam)

buildForAnn :: MonadBuilder n m => ForAnn -> Type n
            -> (Atom n -> m (Atom n)) -> m (Atom n)
buildForAnn ann ty body = fst <$> buildForAnnAux ann ty (\x -> (,()) <$> body x)

buildForAux :: MonadBuilder n m => Direction -> Type n
            -> (Atom n -> m (Atom n, a)) -> m (Atom n, a)
buildForAux = buildForAnnAux . RegularFor

-- Do we need this variant?
buildFor :: MonadBuilder n m => Direction -> Type n
         -> (Atom n -> m (Atom n)) -> m (Atom n)
buildFor = buildForAnn . RegularFor

-- buildNestedFor :: forall n m. MonadBuilder n m
--                => [Direction]
--                -> ClosedBindings (Nest Binder) n
--                -> ([Atom n] -> m (Atom n)) -> m (Atom n)
-- buildNestedFor = undefined
-- buildNestedFor specs body = go specs []
--   where
--     go :: [(Direction, Binder n)] -> [Atom n] -> m (Atom n)
--     go []        indices = body $ reverse indices
--     go ((d,b):t) indices = buildFor d b $ \i -> go t (i:indices)

tabGet :: MonadBuilder n m => Atom n -> Atom n -> m (Atom n)
tabGet tab idx = emit $ App tab idx

tabGetNd :: MonadBuilder n m => Atom n -> [Atom n] -> m (Atom n)
tabGetNd tab idxs = foldM (flip tabGet) tab idxs

unzipTab :: MonadBuilder n m => Atom n -> m (Atom n, Atom n)
unzipTab tab = do
  ~(Pi _ piType) <- getTypeE tab
  let ty = absBinding piType
  fsts <- buildLam ty TabArrow \i -> liftM fst $ app tab i >>= fromPair
  snds <- buildLam ty TabArrow \i -> liftM snd $ app tab i >>= fromPair
  return (fsts, snds)

substBuilderR :: (MonadBuilder o m, MonadReader (Subst i o) m, HasVars e)
            => e i -> m (e o)
substBuilderR x = do
  env <- ask
  substBuilder env x

substBuilder :: (MonadBuilder o m, HasVars e) => Subst i o -> e i -> m (e o)
substBuilder env x = do
  scope <- derivedEnv (const HUnit) . dropEnvOrder <$> getBindings
  return $ subst (env, scope) x

checkBuilder :: (Pretty (e n), HasCallStack, MonadBuilder n m, HasVars e, HasType e)
             => e n -> m (e n)
checkBuilder = undefined
-- checkBuilder x = do
--   bindings <- getBindings
--   eff <- getAllowedEffects
--   case checkType (bindings <> freeVars x) eff x of
--     Left e   -> error $ pprint e
--     Right () -> return x

isSingletonType :: Type n -> Bool
isSingletonType ty = case singletonTypeVal ty of
  Nothing -> False
  Just _  -> True

-- TODO: TypeCon with a single case?
singletonTypeVal :: Type n -> Maybe (Atom n)
-- singletonTypeVal (TabTy v a) = TabValA v <$> singletonTypeVal a
singletonTypeVal (RecordTy (NoExt items)) = Record <$> traverse singletonTypeVal items
singletonTypeVal (TC con) = case con of
  PairType a b -> PairVal <$> singletonTypeVal a <*> singletonTypeVal b
  UnitType     -> return UnitVal
  _            -> Nothing
singletonTypeVal _ = Nothing

indexAsInt :: MonadBuilder n m => Atom n -> m (Atom n)
indexAsInt idx = emitOp $ ToOrdinal idx

instance MonadTrans (BuilderT n) where
  lift m = BuilderT $ lift $ lift m

class Monad m => MonadBuilder n m | m -> n where
  builderLook   :: m (BuilderEnvC n)
  builderExtend :: BuilderEnvC n -> m ()
  builderScoped :: m a -> m (a, BuilderEnvC n)
  builderAsk    :: m (BuilderEnvR n)
  builderLocal  :: (BuilderEnvR n -> BuilderEnvR n) -> m a -> m a

instance Monad m => MonadBuilder n (BuilderT n m) where
  builderLook = BuilderT look
  builderExtend env = BuilderT $ extend env
  builderScoped (BuilderT m) = BuilderT $ scoped m
  builderAsk = BuilderT ask
  builderLocal f (BuilderT m) = BuilderT $ local f m

instance MonadBuilder n m => MonadBuilder n (ReaderT r m) where
  builderLook = lift builderLook
  builderExtend x = lift $ builderExtend x
  builderScoped m = ReaderT \r -> builderScoped $ runReaderT m r
  builderAsk = lift builderAsk
  builderLocal v m = ReaderT \r -> builderLocal v $ runReaderT m r

instance MonadBuilder n m => MonadBuilder n (StateT s m) where
  builderLook = lift builderLook
  builderExtend x = lift $ builderExtend x
  builderScoped m = do
    s <- get
    ((x, s'), env) <- lift $ builderScoped $ runStateT m s
    put s'
    return (x, env)
  builderAsk = lift builderAsk
  builderLocal v m = do
    s <- get
    (x, s') <- lift $ builderLocal v $ runStateT m s
    put s'
    return x

instance (Monoid env, MonadBuilder n m) => MonadBuilder n (CatT env m) where
  builderLook = lift builderLook
  builderExtend x = lift $ builderExtend x
  builderScoped m = do
    env <- look
    ((ans, env'), scopeEnv) <- lift $ builderScoped $ runCatT m env
    extend env'
    return (ans, scopeEnv)
  builderAsk = lift builderAsk
  builderLocal v m = do
    env <- look
    (ans, env') <- lift $ builderLocal v $ runCatT m env
    extend env'
    return ans

instance (Monoid w, MonadBuilder n m) => MonadBuilder n (WriterT w m) where
  builderLook = lift builderLook
  builderExtend x = lift $ builderExtend x
  builderScoped m = do
    ((x, w), env) <- lift $ builderScoped $ runWriterT m
    tell w
    return (x, env)
  builderAsk = lift builderAsk
  builderLocal v m = WriterT $ builderLocal v $ runWriterT m

instance (Monoid env, MonadCat env m) => MonadCat env (BuilderT n m) where
  look = lift look
  extend x = lift $ extend x
  scoped (BuilderT m) = BuilderT $ do
    name <- ask
    env <- look
    ((ans, env'), scopeEnv) <- lift $ lift $ scoped $ runCatT (runReaderT m name) env
    extend env'
    return (ans, scopeEnv)

instance MonadError e m => MonadError e (BuilderT n m) where
  throwError = lift . throwError
  catchError m catch = do
    envC <- builderLook
    envR <- builderAsk
    (ans, envC') <- lift $ runBuilderT' m (envR, envC)
                     `catchError` (\e -> runBuilderT' (catch e) (envR, envC))
    builderExtend envC'
    return ans

instance MonadReader r m => MonadReader r (BuilderT n m) where
  ask = lift ask
  local r m = do
    envC <- builderLook
    envR <- builderAsk
    (ans, envC') <- lift $ local r $ runBuilderT' m (envR, envC)
    builderExtend envC'
    return ans

instance MonadState s m => MonadState s (BuilderT n m) where
  get = lift get
  put = lift . put

askNameStr :: MonadBuilder n m => m NameStr
askNameStr = builderNameHint <$> builderAsk

-- This is purely for human readability. `const id` would be a valid implementation.
withNameHint :: (MonadBuilder n m, HasNameStr a) => a -> m b -> m b
withNameHint hint m = flip builderLocal m \env ->
  env { builderNameHint = getNameStr hint }

runBuilderT' :: Monad m => BuilderT n m a -> BuilderEnv n -> m (a, BuilderEnvC n)
runBuilderT' (BuilderT m) (envR, envC) = runCatT (runReaderT m envR) envC

lookupBinding :: MonadBuilder n m => Name n -> m (Binding n)
lookupBinding = undefined

getBindings :: MonadBuilder n m => m (Bindings n)
getBindings = builderLook

extendBindings :: MonadBuilder n m => Bindings n -> m ()
extendBindings = builderExtend

getAllowedEffects :: MonadBuilder n m => m (EffectRow n)
getAllowedEffects = builderAllowedEffs <$> builderAsk

withEffects :: MonadBuilder n m => EffectRow n -> m a -> m a
withEffects effs m = modifyAllowedEffects (const effs) m

modifyAllowedEffects :: MonadBuilder n m => (EffectRow n -> EffectRow n) -> m a -> m a
modifyAllowedEffects f m = flip builderLocal m \env ->
  env {builderAllowedEffs = f (builderAllowedEffs env) }

extendSourceSubst :: MonadBuilder n m => SourceSubst n -> m a -> m a
extendSourceSubst = undefined
-- extendSourceSubst s m = flip builderLocal m \env ->
--   env { builderSourceSubst = builderSourceSubst env <> s}

getSourceSubst :: MonadBuilder n m => m (SourceSubst n)
getSourceSubst = builderSourceSubst <$> builderAsk

lookupSourceSubst :: MonadBuilder n m => SourceName -> m (Atom n)
lookupSourceSubst v = lookupSubst v <$> getSourceSubst

-- emitDecl :: MonadBuilder n m => Decl n -> m ()
-- emitDecl decl = builderExtend (bindings, Nest decl Empty)
--   where bindings = case decl of
--           Let ann b expr -> b @> (binderType b, LetBound ann expr)

liftBuilder :: MonadBuilder n m => Builder n a -> m a
liftBuilder action = do
  envR <- builderAsk
  envC <- builderLook
  let (ans, envC') = runIdentity $ runBuilderT' action (envR, envC)
  builderExtend envC'
  return ans

-- === generic traversal ===

type TraversalDef i o m = ( Decl i -> m (Subst i o)
                          , Expr i -> m (Expr o)
                          , Atom i -> m (Atom o))

substTraversalDef :: (MonadBuilder o m, MonadReader (Subst i o) m)
                  => TraversalDef i o m
substTraversalDef = ( traverseDecl substTraversalDef
                    , traverseExpr substTraversalDef
                    , traverseAtom substTraversalDef
                    )

appReduceTraversalDef :: (MonadBuilder o m, MonadReader (Subst i o) m)
                      => TraversalDef i o m
appReduceTraversalDef = ( traverseDecl appReduceTraversalDef
                        , reduceAppExpr
                        , traverseAtom appReduceTraversalDef)
  where
    reduceAppExpr expr = case expr of
      App f' x' -> do
        f <- traverseAtom appReduceTraversalDef f'
        x <- traverseAtom appReduceTraversalDef x'
        case f of
          -- TabVal b body ->
          --   Atom <$> (dropSub $ extendR (b@>x) $ evalBlockE appReduceTraversalDef body)
          _ -> return $ App f x
      _ -> traverseExpr appReduceTraversalDef expr

-- With `def = (traverseExpr def, traverseAtom def)` this should be a no-op
-- traverseDecls :: (MonadBuilder o m, MonadReader (Subst i o) m)
--               => TraversalDef i o m -> Nest Decl i -> m (Nest Decl o, Subst i o)
-- traverseDecls = undefined
-- traverseDecls def decls = liftM swap $ scopedDecls $ traverseDeclsOpen def decls

-- traverseDeclsOpen :: (MonadBuilder o m, MonadReader (Subst i o) m)
--                   => TraversalDef i o m -> Nest Decl i -> m (Subst i o)
-- traverseDeclsOpen = undefined
-- traverseDeclsOpen _ Empty = return mempty
-- traverseDeclsOpen def@(fDecl, _, _) (Nest decl decls) = do
--   env <- fDecl decl
--   env' <- extendR env $ traverseDeclsOpen def decls
--   return (env <> env')

traverseDecl :: (MonadBuilder o m, MonadReader (Subst i o) m)
             => TraversalDef i o m -> Decl i -> m (Subst i o)
traverseDecl = undefined
-- traverseDecl (_, fExpr, _) decl = case decl of
  -- Let letAnn b expr -> do
  --   expr' <- fExpr expr
  --   case expr' of
  --     Atom (Var v) -> return $ b @> (Var v)
  --     _ -> (b@>) <$> emitAnn letAnn expr'

traverseBlock :: (MonadBuilder o m, MonadReader (Subst i o) m)
              => TraversalDef i o m -> (Block i) -> m (Block o)
traverseBlock def block = buildScopedBlock $ evalBlockE def block

evalBlockE :: (MonadBuilder o m, MonadReader (Subst i o) m)
              => TraversalDef i o m -> Block i -> m (Atom o)
evalBlockE = undefined
-- evalBlockE def@(_, fExpr, _) (Block decls result) = do
--   env <- traverseDeclsOpen def decls
--   resultExpr <- extendR env $ fExpr result
--   case resultExpr of
--     Atom a -> return a
--     _      -> emit resultExpr

traverseExpr :: (MonadBuilder o m, MonadReader (Subst i o) m)
             => TraversalDef i o m -> Expr i -> m (Expr o)
traverseExpr def@(_, _, fAtom) expr = case expr of
  App g x -> App  <$> fAtom g <*> fAtom x
  Atom x  -> Atom <$> fAtom x
  Op  op  -> Op   <$> traverse fAtom op
  Hof hof -> Hof  <$> traverse fAtom hof
  -- Case e alts ty -> Case <$> fAtom e <*> mapM traverseAlt alts <*> fAtom ty
  -- where
  --   traverseAlt (Abs bs body) = do
  --     bs' <- mapM (mapM fAtom) bs
  --     buildNAbs bs' \xs -> extendR (newEnv bs' xs) $ evalBlockE def body

traverseAtom :: forall i o m . (MonadBuilder o m, MonadReader (Subst i o) m)
             => TraversalDef i o m -> Atom i -> m (Atom o)
traverseAtom = undefined
-- traverseAtom def@(_, _, fAtom) atom = case atom of
--   Var _ -> substBuilderR atom
--   -- Lam (Abs b (WithArrow arr body)) -> do
--   --   b' <- mapM fAtom b
--   --   buildDepEffLam b'
--   --     (\x -> extendR (b'@>x) (substBuilderR arr))
--   --     (\x -> extendR (b'@>x) (evalBlockE def body))
--   Pi _ -> substBuilderR atom
--   Con con -> Con <$> traverse fAtom con
--   TC  tc  -> TC  <$> traverse fAtom tc
--   Eff _   -> substBuilderR atom
--   -- DataCon dataDef params con args -> DataCon dataDef <$>
--   --   traverse fAtom params <*> pure con <*> traverse fAtom args
--   -- TypeCon dataDef params -> TypeCon dataDef <$> traverse fAtom params
--   -- LabeledRow (Ext items rest) -> do
--   --   items' <- traverse fAtom items
--   --   return $ LabeledRow $ Ext items' rest
--   -- Record items -> Record <$> traverse fAtom items
--   -- RecordTy (Ext items rest) -> do
--   --   items' <- traverse fAtom items
--   --   return $ RecordTy $ Ext items' rest
--   -- Variant (Ext types rest) label i value -> do
--   --   types' <- traverse fAtom types
--   --   Variant (Ext types' rest) label i <$> fAtom value
--   -- VariantTy (Ext items rest) -> do
--   --   items' <- traverse fAtom items
--   --   return $ VariantTy $ Ext items' rest
--   ACase e alts ty -> ACase <$> fAtom e <*> mapM traverseAAlt alts <*> fAtom ty
--   -- DataConRef dataDef params args -> DataConRef dataDef <$>
--   --   traverse fAtom params <*> traverseNestedArgs args
--   -- BoxedRef b ptr size body -> do
--   --   ptr'  <- fAtom ptr
--   --   size' <- buildScoped $ evalBlockE def size
--   --   (Abs _ decls, Abs b' body') <- buildAbs b \x ->
--   --     extendR (b@>x) $ evalBlockE def (Block Empty $ Atom body)
--   --   case decls of
--   --     Empty -> return $ BoxedRef b' ptr' size' body'
--   --     _ -> error "Traversing the body atom shouldn't produce decls"
--   ProjectElt _ _ -> substBuilderR atom
--   where
--     traverseNestedArgs :: Nest DataConRefBinding i -> m (Nest DataConRefBinding o)
--     traverseNestedArgs Empty = return Empty
--     -- traverseNestedArgs (Nest (DataConRefBinding b ref) rest) = do
--     --   ref' <- fAtom ref
--     --   b' <- substBuilderR b
--     --   v <- freshVarE UnknownBinder b'
--     --   rest' <- extendR (b @> Var v) $ traverseNestedArgs rest
--     --   return $ Nest (DataConRefBinding (Bind v) ref') rest'

--     traverseAAlt = undefined
--     -- traverseAAlt (Abs bs a) = do
--     --   bs' <- mapM (mapM fAtom) bs
--     --   (Abs bs'' b) <- buildNAbs bs' \xs -> extendR (newEnv bs' xs) $ fAtom a
--     --   case b of
--     --     Block Empty (Atom r) -> return $ Abs bs'' r
--     --     _                    -> error "ACase alternative traversal has emitted decls or exprs!"

transformModuleAsBlock :: (Block i -> Block o) -> Module i -> Module o
transformModuleAsBlock = undefined
-- transformModuleAsBlock transform (Module ir decls bindings) = do
--   let localVars = filter (not . isGlobal . varName) $ bindingsAsVars $ freeVars bindings
--   let block = Block decls $ Atom $ mkConsList $ map Var localVars
--   let (Block newDecls (Atom newResult)) = transform block
--   let newLocalVals = ignoreExcept $ fromConsList newResult
--   Module ir newDecls $ scopelessSubst (newEnv localVars newLocalVals) bindings

indexSetSizeE :: MonadBuilder n m => Type n -> m (Atom n)
indexSetSizeE (TC con) = case con of
  UnitType                   -> return $ IdxRepVal 1
  IntRange low high -> clampPositive =<< high `isub` low
  IndexRange n low high -> do
    low' <- case low of
      InclusiveLim x -> indexToIntE x
      ExclusiveLim x -> indexToIntE x >>= iadd (IdxRepVal 1)
      Unlimited      -> return $ IdxRepVal 0
    high' <- case high of
      InclusiveLim x -> indexToIntE x >>= iadd (IdxRepVal 1)
      ExclusiveLim x -> indexToIntE x
      Unlimited      -> indexSetSizeE n
    clampPositive =<< high' `isub` low'
  PairType a b -> bindM2 imul (indexSetSizeE a) (indexSetSizeE b)
  ParIndexRange _ _ _ -> error "Shouldn't be querying the size of a ParIndexRange"
  _ -> error $ "Not implemented " ++ pprint con
indexSetSizeE (RecordTy (NoExt types)) = do
  sizes <- traverse indexSetSizeE types
  foldM imul (IdxRepVal 1) sizes
indexSetSizeE (VariantTy (NoExt types)) = do
  sizes <- traverse indexSetSizeE types
  foldM iadd (IdxRepVal 0) sizes
indexSetSizeE ty = error $ "Not implemented " ++ pprint ty

clampPositive :: MonadBuilder n m => Atom n -> m (Atom n)
clampPositive x = do
  isNegative <- x `ilt` (IdxRepVal 0)
  select isNegative (IdxRepVal 0) x

-- XXX: Be careful if you use this function as an interpretation for
--      IndexAsInt instruction, as for Int and IndexRanges it will
--      generate the same instruction again, potentially leading to an
--      infinite loop.
indexToIntE :: MonadBuilder n m => Atom n -> m (Atom n)
indexToIntE (Con (IntRangeVal _ _ i))     = return i
indexToIntE (Con (IndexRangeVal _ _ _ i)) = return i
indexToIntE idx = do
  idxTy <- getTypeE idx
  case idxTy of
    UnitTy  -> return $ IdxRepVal 0
    PairTy _ rType -> do
      (lVal, rVal) <- fromPair idx
      lIdx  <- indexToIntE lVal
      rIdx  <- indexToIntE rVal
      rSize <- indexSetSizeE rType
      imul rSize lIdx >>= iadd rIdx
    TC (IntRange _ _)     -> indexAsInt idx
    TC (IndexRange _ _ _) -> indexAsInt idx
    TC (ParIndexRange _ _ _) -> error "Int casts unsupported on ParIndexRange"
    RecordTy (NoExt types) -> do
      sizes <- traverse indexSetSizeE types
      (strides, _) <- scanM (\sz prev -> (prev,) <$> imul sz prev) sizes (IdxRepVal 1)
      -- Unpack and sum the strided contributions
      subindices <- getUnpacked idx
      subints <- traverse indexToIntE subindices
      scaled <- mapM (uncurry imul) $ zip (toList strides) subints
      foldM iadd (IdxRepVal 0) scaled
    -- VariantTy (NoExt types) -> do
    --   sizes <- traverse indexSetSizeE types
    --   (offsets, _) <- scanM (\sz prev -> (prev,) <$> iadd sz prev) sizes (IdxRepVal 0)
    --   -- Build and apply a case expression
    --   alts <- flip mapM (zip (toList offsets) (toList types)) $
    --     \(offset, subty) -> buildNAbs (toNest [Ignore subty]) \[subix] -> do
    --       i <- indexToIntE subix
    --       iadd offset i
    -- --   emit $ Case idx alts IdxRepTy
    ty -> error $ "Unexpected type " ++ pprint ty

intToIndexE :: MonadBuilder n m => Type n -> Atom n -> m (Atom n)
intToIndexE (TC con) i = case con of
  IntRange        low high   -> return $ Con $ IntRangeVal        low high i
  IndexRange from low high   -> return $ Con $ IndexRangeVal from low high i
  UnitType                   -> return $ UnitVal
  PairType a b -> do
    bSize <- indexSetSizeE b
    iA <- intToIndexE a =<< idiv i bSize
    iB <- intToIndexE b =<< irem i bSize
    return $ PairVal iA iB
  ParIndexRange _ _ _ -> error "Int casts unsupported on ParIndexRange"
  _ -> error $ "Unexpected type " ++ pprint con
intToIndexE (RecordTy (NoExt types)) i = do
  sizes <- traverse indexSetSizeE types
  (strides, _) <- scanM
    (\sz prev -> do {v <- imul sz prev; return ((prev, v), v)}) sizes (IdxRepVal 1)
  offsets <- flip mapM (zip (toList types) (toList strides)) $
    \(ty, (s1, s2)) -> do
      x <- irem i s2
      y <- idiv x s1
      intToIndexE ty y
  return $ Record (restructure offsets types)
intToIndexE (VariantTy (NoExt types)) i = do
  sizes <- traverse indexSetSizeE types
  (offsets, _) <- scanM (\sz prev -> (prev,) <$> iadd sz prev) sizes (IdxRepVal 0)
  let
    reflect = reflectLabels types
    -- Find the right index by looping through the possible offsets
    go prev ((label, repeatNum), ty, offset) = do
      shifted <- isub i offset
      -- TODO: This might run intToIndex on negative indices. Fix this!
      index   <- intToIndexE ty shifted
      beforeThis <- ilt i offset
      select beforeThis prev $ Variant (NoExt types) label repeatNum index
    ((l0, 0), ty0, _):zs = zip3 (toList reflect) (toList types) (toList offsets)
  start <- Variant (NoExt types) l0 0 <$> intToIndexE ty0 i
  foldM go start zs
intToIndexE ty _ = error $ "Unexpected type " ++ pprint ty

-- === Helpers for function evaluation over fixed-width types ===

applyIntBinOp' :: (forall a. (Eq a, Ord a, Num a, Integral a) => (a -> Atom n) -> a -> a -> Atom n)
               -> Atom n -> Atom n -> Atom n
applyIntBinOp' f x y = case (x, y) of
  (Con (Lit (Int64Lit xv)), Con (Lit (Int64Lit yv))) -> f (Con . Lit . Int64Lit) xv yv
  (Con (Lit (Int32Lit xv)), Con (Lit (Int32Lit yv))) -> f (Con . Lit . Int32Lit) xv yv
  (Con (Lit (Word8Lit xv)), Con (Lit (Word8Lit yv))) -> f (Con . Lit . Word8Lit) xv yv
  _ -> error "Expected integer atoms"

applyIntBinOp :: (forall a. (Num a, Integral a) => a -> a -> a)
              -> Atom n -> Atom n -> Atom n
applyIntBinOp f x y = applyIntBinOp' (\w -> w ... f) x y

applyIntCmpOp :: (forall a. (Eq a, Ord a) => a -> a -> Bool)
              -> Atom n -> Atom n -> Atom n
applyIntCmpOp f x y = applyIntBinOp' (\_ -> (Con . Lit . Word8Lit . fromIntegral . fromEnum) ... f) x y

applyFloatBinOp :: (forall a. (Num a, Fractional a) => a -> a -> a)
                -> Atom n -> Atom n -> Atom n
applyFloatBinOp f x y = case (x, y) of
  (Con (Lit (Float64Lit xv)), Con (Lit (Float64Lit yv))) -> Con $ Lit $ Float64Lit $ f xv yv
  (Con (Lit (Float32Lit xv)), Con (Lit (Float32Lit yv))) -> Con $ Lit $ Float32Lit $ f xv yv
  _ -> error "Expected float atoms"

applyFloatUnOp :: (forall a. (Num a, Fractional a) => a -> a) -> Atom n -> Atom n
applyFloatUnOp f x = applyFloatBinOp (\_ -> f) undefined x
