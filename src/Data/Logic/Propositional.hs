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

import Data.Logic.Atomic
import Data.Logic.Boolean
import Data.Logic.Propositional.Class
import Data.Logic.Decidable

import Control.Applicative

data Prop r
  = Prop r
  | TT
  | FF
  | Not  (Prop r)
  | Prop r :&: Prop r
  | Prop r :|: Prop r
  deriving (Eq,Ord,Show)
infixr 3 :&:
infixr 2 :|:

instance Propositional (Prop r) where
  neg   = \case
    Not p -> p
    TT    -> FF
    FF    -> TT
    p     -> Not p
  p .|. q = case (p,q) of
    (TT,_ ) -> TT
    (_ ,TT) -> TT
    (FF,_ ) -> q
    (_ ,FF) -> p
    _       -> p :|: q
  p .&. q = case (p,q) of
    (FF,_ ) -> FF
    (_ ,FF) -> FF
    (TT,_ ) -> q
    (_ ,TT) -> p
    _       -> p :&: q

instance Boolean (Prop r) where
  tt = TT
  ff = FF

instance Atomic a r => Atomic a (Prop r) where
  atom = Prop . atom

instance Decidable r => Decidable (Prop r) where
  type Decide (Prop r) = Decide r
  truth = \case
    Prop p   -> truth p
    TT       -> pure True
    FF       -> pure False
    Not  p   -> fmap not $ truth p
    p :|: q -> (||) <$> truth p <*> truth q
    p :&: q -> (&&) <$> truth p <*> truth q

test' :: Prop (Atom String) -> IO ()
test' = print

