{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Logic.Atomic where

import Data.Logic.Atomic.Class
import Data.Logic.Decidable

import Test.QuickCheck (Arbitrary(..),oneof)

import Control.Applicative
import Data.Functor.Compose
import Control.Monad.Reader.Class
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Atom a r
  = Atom a
  | NonAtom r
  deriving (Eq,Ord,Show)

instance (Arbitrary a, Arbitrary r) => Arbitrary (Atom a r) where
  arbitrary = oneof
    [ Atom    <$> arbitrary
    , NonAtom <$> arbitrary
    ]
  shrink = \case
    Atom    a -> [Atom    b | b <- shrink a]
    NonAtom p -> [NonAtom q | q <- shrink p]

instance Atomic a (Atom a r) where
  atom = Atom

instance (Decidable r, Atomic a r, Ord a) => Decidable (Atom a r) where
  type Decide (Atom a r) = (->) (Map a r) `Compose` Decide r
  truth = Compose . \case
    Atom a    -> truth . fromMaybe (atom a) <$> asks (M.lookup a)
    NonAtom p -> return $ truth p

