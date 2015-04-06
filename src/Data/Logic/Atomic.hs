{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Logic.Atomic where

import Test.QuickCheck (Arbitrary(..),oneof)

import Control.Applicative

class Atomic a r | r -> a where
  atom   :: a -> r

newtype Atom a = Atom a
  deriving (Eq,Ord,Show)

instance Arbitrary a => Arbitrary (Atom a) where
  arbitrary = Atom <$> arbitrary

instance Atomic a (Atom a) where
  atom = Atom

data AnyAtomic a = AnyAtomic
  { getAtomic :: forall r. Atomic a r => r
  }

instance Atomic a (AnyAtomic a) where
  atom a = AnyAtomic $ atom a

