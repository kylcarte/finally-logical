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

import Data.Logic.Propositional.Class
import Data.Logic.Decidable

import Test.QuickCheck (Arbitrary(..),oneof)

import Control.Applicative

data Prop r
  = Prop r
  | Neg  (Prop r)
  | Prop r :&: Prop r
  | Prop r :|: Prop r
  deriving (Eq,Ord,Show)
infixr 3 :&:
infixr 2 :|:

instance Arbitrary r => Arbitrary (Prop r) where
  arbitrary = oneof
    [ Prop  <$> arbitrary
    , Neg   <$> arbitrary
    , (:&:) <$> arbitrary <*> arbitrary
    , (:|:) <$> arbitrary <*> arbitrary
    ]
  shrink = \case
    Prop _  -> []
    Neg  p  -> [p]   ++ [ Neg q   | q <- shrink p ]
    p :|: q -> [p,q] ++ [ r :|: s | r <- shrink p, s <- shrink q ]
    p :&: q -> [p,q] ++ [ r :&: s | r <- shrink p, s <- shrink q ]

instance Propositional (Prop r) where
  neg   = Neg
  (.|.) = (:|:)
  (.&.) = (:&:)

instance Decidable r => Decidable (Prop r) where
  type Decide (Prop r) = Decide r
  truth = \case
    Prop p   -> truth p
    Neg  p   -> fmap not $ truth p
    p :|: q -> (||) <$> truth p <*> truth q
    p :&: q -> (&&) <$> truth p <*> truth q

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

