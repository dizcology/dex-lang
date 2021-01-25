-- Copyright 2021 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Naming (
  NameStr, Name, Env, (!), emptyEnv, Ext, liftNS,
  HasNames (..), AlphaEq (..),
  Abs, makeAbs, withAbs, pattern ConstAbs,
  NaryAbs, makeNaryAbs, withNaryAbs,
  BuilderT, runBuilderT, MonadBuilder (..),
  NameMap, lookupNameMap, (@>), NameSubst, Scope,
  Bindings, AnnAbs, NestedAbs (..), NestedNaryAbs (..),
  PrettyH (..), NameHint, SourceNS, SourceName, SourceNameSubst,
  ) where


import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Store (Store)
import Data.String
import Data.Void (Void)
import GHC.Generics


import HigherKinded

-- === core API ===

newtype NameStr = NameStr T.Text  deriving (Show, Ord, Eq, Generic)
data Name n = Name NameStr Int    deriving (Show, Ord, Eq, Generic)

newtype Env n a = UnsafeMakeEnv (NameMap n a)
                  deriving (Show, Eq, Ord, Generic)

(!) :: Env n a -> Name n -> a
(!) (UnsafeMakeEnv m) v = case lookupNameMap v m of
  Nothing -> error "This shouldn't be possible!"
  Just x -> x

type NameHint = NameStr
type NameSubst i o = Env i (Name o)
type Scope n = Env n ()

class HasNames e where
  freeNames      :: e n -> NameMap n ()
  applyNameSubst :: e i -> Scope o -> NameSubst i o -> e o

class HasNames e => AlphaEq e where
  alphaEq :: Scope n -> e n -> e n -> Bool

unsafeCoerceNames :: HasNames e => e i -> e o
unsafeCoerceNames = undefined

unsafeCoerceEnv :: Env i a -> Env o a
unsafeCoerceEnv = undefined

emptyEnv :: Env Void a
emptyEnv = UnsafeMakeEnv mempty

data Ext old new  -- extended namespace

liftNS :: HasNames e => e n -> e (Ext n ext)
liftNS = unsafeCoerceNames

unsafeFreshName :: NameHint -> NameMap n a -> Name n
unsafeFreshName hint m = undefined

-- === source names ===

data SourceNS
type SourceName = Name SourceNS

data SourceNameSubst n = SourceNameSubst (NameSubst SourceNS n)
                         deriving (Show, Generic)

instance IsString (Name SourceNS) where
  fromString = undefined

-- === abstraction ===

data Abs e n = UnsafeMakeAbs (Maybe (Name n)) (e n)
               deriving (Show, Generic)

-- TODO: handle the `Just` version too, checking the name isn't free in the body
pattern ConstAbs :: body n -> Abs body n
pattern ConstAbs body = UnsafeMakeAbs Nothing body

makeAbs :: HasNames e
        => NameHint -> Env n a -> a
        -> (forall ext. Env (Ext n ext) a -> Name (Ext n ext) -> e (Ext n ext))
        -> Abs e n
makeAbs hint (UnsafeMakeEnv env) binding action =
  UnsafeMakeAbs (Just v) $ unsafeCoerceNames $
    action (unsafeCoerceEnv env') (unsafeCoerceNames v)
  where
    v = unsafeFreshName hint env
    env' = UnsafeMakeEnv $ env <> (v @> binding)

withAbs :: Env n a -> Abs e n -> a -> (forall n'. Env n' a -> e n' -> b) -> b
withAbs (UnsafeMakeEnv env) (UnsafeMakeAbs v body) binding action =
  action env' body
  where env' = UnsafeMakeEnv $ env <> foldMap (@> binding) v

-- === nary abstraction ===

data NaryAbs e n = UnsafeMakeNaryAbs [Maybe (Name n)] (e n)
                   deriving (Show, Generic)

makeNaryAbs :: HasNames e
            => [NameHint] -> Env n a -> [a]
            -> (forall ext. Env (Ext n ext) a -> [Name (Ext n ext)] -> e (Ext n ext))
            -> NaryAbs e n
makeNaryAbs hint (UnsafeMakeEnv env) bindings action = undefined

withNaryAbs :: Env n a -> NaryAbs e n -> [a]
            -> (forall n'. Env n' a -> e n' -> b)
            -> b
withNaryAbs (UnsafeMakeEnv env) (UnsafeMakeNaryAbs v body) binding action = undefined

-- === nested abstraction ===

data NestedAbs bindings body n =
   NestedAbs (bindings n) (Abs (NestedAbs bindings body) n)
 | NestedAbsResult (body n)
   deriving (Show, Generic)

data NestedNaryAbs bindings body n =
   NestedNaryAbs (bindings n) (NaryAbs (NestedNaryAbs bindings body) n)
 | NestedNaryAbsResult (body n)
   deriving (Show, Generic)

-- === builder monad ===

newtype BuilderT b n m a =
  BuilderT (StateT [b n] m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, Alternative)

runBuilderT :: Monad m
            => Env n (b n)
            -> (forall ext. BuilderT b (Ext n ext) m (e (Ext n ext)))
            -> NestedAbs b e n
runBuilderT _ _ = undefined

class Monad m => MonadBuilder b n m | m -> n, m -> b where
  lookupBinding :: Name n -> m (b n)
  emitBinding   :: b n -> m (Name n)
  withNameHint  :: NameHint -> m a -> m a

instance Monad m => MonadBuilder b n (BuilderT b n m) where
  lookupBinding = undefined
  emitBinding   = undefined
  withNameHint  = undefined

-- === NameMap (like Env, but without the totality guarantee) ===

newtype NameMap n a = NameMap (M.Map (Name n) a)
                      deriving (Show, Eq, Ord, Generic)

lookupNameMap :: Name n -> NameMap n a -> Maybe a
lookupNameMap = undefined

instance Semigroup (NameMap n a) where
  NameMap m <> NameMap m' = NameMap (m' <> m)

instance Monoid (NameMap n a) where
  mempty = NameMap mempty
  mappend = (<>)

infixr 7 @>
(@>) :: Name n -> a -> NameMap n a
v @> x = NameMap $ M.singleton v x

-- === convenience utilities ===

type Bindings e n = Env n (e n)

type AnnAbs b e = PairH b (Abs e)

-- === generic instances ===










-- === instances ===

instance PrettyH Name where
  prettyH = undefined

instance HasNames Name where
  freeNames      = undefined
  applyNameSubst = undefined

instance Store NameStr
instance Store (Name n)
instance (Store n, Store (body n)) => Store (Abs  body n)
instance (Store n, Store (b n), Store (body n)) => Store (NestedAbs b body n)
