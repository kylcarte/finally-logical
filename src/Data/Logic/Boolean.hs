{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Data.Logic.Boolean
  ( module Data.Logic.Boolean
  , module Data.Logic.Boolean.Class
  ) where

import Data.Logic.Atomic.Class
import Data.Logic.Boolean.Class
import Data.Logic.Propositional.Class
import Data.Logic.Modal.Class

import Data.Logic.Decidable
import Data.Logic.Embed
import Data.Logic.Util

import Test.QuickCheck (Arbitrary(..),oneof)

data BoolI r
  = TT
  | FF
  | LiftBool r
  deriving (Eq,Ord,Show)

instance Arbitrary r => Arbitrary (BoolI r) where
  arbitrary = oneof
    [ pure TT
    , pure FF
    , LiftBool <$> arbitrary
    ]
  shrink = \case
    LiftBool p -> [LiftBool q | q <- shrink p]
    _          -> []

instance Boolean r => Embed r (BoolI r) where
  embed = LiftBool
  lower = \case
    TT         -> tt
    FF         -> ff
    LiftBool p -> p

instance (Boolean r, EmbedStar a r) => EmbedStar a (BoolI r)

instance Contextual r (BoolI r) where
  type Context (BoolI r) = Either Bool
  embedCxt = either (TT ? FF) LiftBool
  lowerCxt = \case
    TT         -> Left True
    FF         -> Left False
    LiftBool p -> Right p

instance ContextStar a r => ContextStar a (BoolI r) where
  type AllContext (BoolI r) = CompAllCxt r (BoolI r)

instance Decidable r => Decidable (BoolI r) where
  type Decide (BoolI r) = Decide r
  truth = either pure truth . lowerCxt

instance Atomic a r => Atomic a (BoolI r) where
  atom = LiftBool . atom

instance Boolean (BoolI r) where
  tt = TT
  ff = FF

instance (Boolean r, Propositional r) => Propositional (BoolI r) where
  neg    = overLower  neg
  (.|.)  = overLower2 (.|.)
  (.^.)  = overLower2 (.^.)
  (.&.)  = overLower2 (.&.)
  (.->.) = overLower2 (.->.)
  (.==.) = overLower2 (.==.)

instance (Boolean r, Modal m r) => Modal m (BoolI r) where
  square  = overLower . square
  diamond = overLower . diamond

