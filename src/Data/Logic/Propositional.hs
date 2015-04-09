{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Data.Logic.Propositional
  ( module Data.Logic.Propositional
  , module Data.Logic.Propositional.Class
  ) where

import Data.Logic.Atomic.Class
import Data.Logic.Boolean.Class
import Data.Logic.Decidable
import Data.Logic.Embed
import Data.Logic.Propositional.Class
import Data.Logic.Modal.Class

import Test.QuickCheck (Arbitrary(..),frequency)

import Control.Applicative

data PropI r
  = Neg  (PropI r)
  | PropI r :&: PropI r
  | PropI r :|: PropI r
  | LiftProp r
  deriving (Eq,Ord,Show)
infixr 3 :&:
infixr 2 :|:

instance Arbitrary r => Arbitrary (PropI r) where
  arbitrary = frequency
    [ ( 2 , Neg      <$> arbitrary )
    , ( 2 , LiftProp <$> arbitrary )
    , ( 1 , (:|:) <$> arbitrary <*> arbitrary )
    , ( 1 , (:&:) <$> arbitrary <*> arbitrary )
    ]
  shrink = \case
    Neg  p     -> [p]   ++ [ Neg q   | q <- shrink p ]
    p :|: q    -> [p,q] ++ [ r :|: s | r <- shrink p, s <- shrink q ]
    p :&: q    -> [p,q] ++ [ r :&: s | r <- shrink p, s <- shrink q ]
    LiftProp p -> [LiftProp q | q <- shrink p]

instance Atomic a r => Atomic a (PropI r) where
  atom = LiftProp . atom

instance Boolean r => Boolean (PropI r) where
  tt = LiftProp tt
  ff = LiftProp ff

instance Decidable r => Decidable (PropI r) where
  type Decide (PropI r) = Decide r
  truth = \case
    Neg  p     -> fmap not $ truth p
    p :|: q    -> (||) <$> truth p <*> truth q
    p :&: q    -> (&&) <$> truth p <*> truth q
    LiftProp p -> truth p

instance Propositional r => Embed r (PropI r) where
  lower = \case
    Neg  p     -> neg $ lower p
    p :|: q    -> lower p .|. lower q
    p :&: q    -> lower p .&. lower q
    LiftProp p -> p
  embed = LiftProp

instance (Propositional r, EmbedStar a r) => EmbedStar a (PropI r)

instance Propositional (PropI r) where
  neg   = Neg
  (.|.) = (:|:)
  (.&.) = (:&:)

instance Modal m r => Modal m (PropI r) where
  square  m = \case
    Neg   p    -> Neg  $ diamond m p
    p :|: q    -> embed $ square  m $ lower p .|. lower q
    p :&: q    -> embed $ square  m $ lower p .&. lower q
    LiftProp p -> embed $ square  m p
  diamond m = \case
    Neg   p    -> Neg  $ square  m p
    p :|: q    -> embed $ diamond m $ lower p .|. lower q
    p :&: q    -> embed $ diamond m $ lower p .&. lower q
    LiftProp p -> embed $ diamond m p

data AnyProp = AnyProp
  { getProp :: forall r. Propositional r => r
  }

instance Propositional AnyProp where
  neg p    = AnyProp $ neg $ getProp p
  p .|. q  = AnyProp $ getProp p .|.  getProp q
  p .&. q  = AnyProp $ getProp p .&.  getProp q
  p .^. q  = AnyProp $ getProp p .^.  getProp q
  p .->. q = AnyProp $ getProp p .->. getProp q
  p .==. q = AnyProp $ getProp p .==. getProp q

