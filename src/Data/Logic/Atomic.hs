{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Logic.Atomic
  ( module Data.Logic.Atomic
  , module Data.Logic.Atomic.Class
  ) where

import Data.Logic.Atomic.Class
import Data.Logic.Boolean.Class
import Data.Logic.Decidable
import Data.Logic.Embed
import Data.Logic.Util

import Test.QuickCheck (Arbitrary(..),oneof)

import Control.Monad.Reader.Class
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data AtomI a r
  = Atom a
  | LiftAtom r
  deriving (Eq,Ord,Show)

instance (Arbitrary a, Arbitrary r) => Arbitrary (AtomI a r) where
  arbitrary = oneof
    [ Atom     <$> arbitrary
    , LiftAtom <$> arbitrary
    ]
  shrink = \case
    Atom     a -> [Atom     b | b <- shrink a]
    LiftAtom p -> [LiftAtom q | q <- shrink p]

instance Atomic a r => Embed r (AtomI a r) where
  lower = \case
    Atom     a -> atom a
    LiftAtom p -> p
  embed = LiftAtom

instance (Atomic a r, EmbedStar b r) => EmbedStar b (AtomI a r)

instance Contextual r (AtomI a r) where
  type Context (AtomI a r) = Either a
  embedCxt = either Atom LiftAtom
  lowerCxt = \case
    Atom     a -> Left  a
    LiftAtom p -> Right p

instance ContextStar b r => ContextStar b (AtomI a r) where
  type AllContext (AtomI a r) = CompAllCxt r (AtomI a r)

instance Atomic a (AtomI a r) where
  atom = Atom

instance Boolean r => Boolean (AtomI a r) where
  tt = LiftAtom tt
  ff = LiftAtom ff

instance (Decidable r, Atomic a r, Ord a) => Decidable (AtomI a r) where
  type Decide (AtomI a r) = Compose ((->) (Map a r)) (Decide r)
  truth = Compose . \case
    Atom a    -> truth . fromMaybe (atom a) <$> asks (M.lookup a)
    LiftAtom p -> return $ truth p

