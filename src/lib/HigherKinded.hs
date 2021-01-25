-- Copyright 2021 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE DeriveGeneric #-}

module HigherKinded (
  PairH (..), ListH (..), LiftH (..), MaybeH (..),
  UnitH, pattern UnitH, fstH, sndH,
  lengthH, TraversableH (..), mapMH_, fmapH, voidH,
  toListH, nullH, PrettyH (..))
  where

import Control.Monad.Identity
import Data.Store (Store)
import Data.Text.Prettyprint.Doc
import GHC.Generics

data    PairH e1 e2 n = PairH (e1 n) (e2 n)     deriving (Show, Eq, Generic)
newtype MaybeH e n = MaybeH {fromMaybeH :: Maybe (e n) } deriving (Show, Eq, Generic)
newtype ListH e n = ListH {fromListH :: [e n] } deriving (Show, Eq, Generic)
newtype LiftH a n = LiftH {fromLiftH :: a}      deriving (Show, Eq, Generic)

type UnitH = LiftH ()

pattern UnitH :: UnitH n
pattern UnitH = LiftH ()

fstH :: PairH a b n -> a n
fstH (PairH x _) = x

sndH :: PairH a b n -> b n
sndH (PairH _ x) = x

-- === higher-kinded traversable ===

class TraversableH f where
  mapMH :: Applicative m => (e1 n1 -> m (e2 n2)) -> f e1 n1 -> m (f e2 n2)

mapMH_ :: (TraversableH f, Applicative m)
       => (e n -> m ()) -> f e n -> m ()
mapMH_ f xs = void $ mapMH (const (pure UnitH) . f) xs

fmapH :: TraversableH f => (e1 n1 -> e2 n2) -> f e1 n1 -> f e2 n2
fmapH f xs = runIdentity $ mapMH (return . f) xs

voidH :: TraversableH f => f e n1 -> f UnitH n2
voidH xs = fmapH (const UnitH) xs

toListH :: TraversableH f => f e n -> [e n]
toListH = undefined

nullH :: TraversableH f => f e n -> Bool
nullH = null . toListH

lengthH :: TraversableH f => f e n -> Int
lengthH xs = length $ toListH xs

-- === instances ===

class PrettyH e where
  prettyH :: e n -> Doc ann

instance TraversableH ListH where
  mapMH = undefined

instance (Store e) => Store (LiftH e n)
instance (Store n, Store (e n)) => Store (ListH e n)
instance (Store n, Store (e1 n), Store (e2 n)) => Store (PairH e1 e2 n)
