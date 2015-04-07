{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Data.Logic.Boolean where

import Data.Logic.Boolean.Class
import Data.Logic.Decidable

import Control.Applicative

data Boolean' r
  = TT
  | FF
  | NonBool r
  deriving (Eq,Ord,Show)

instance Boolean (Boolean' r) where
  tt = TT
  ff = FF

instance Decidable r => Decidable (Boolean' r) where
  type Decide (Boolean' r) = Decide r
  truth = \case
    TT        -> pure True
    FF        -> pure False
    NonBool p -> truth p

