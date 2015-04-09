{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Logic.Modal
  ( module Data.Logic.Modal
  , module Data.Logic.Modal.Class
  ) where

import Data.Logic.Atomic.Class
import Data.Logic.Boolean.Class
import Data.Logic.Modal.Class
import Data.Logic.Propositional.Class
import Data.Logic.Decidable
import Data.Logic.Embed
import Data.Logic.Util

import Control.Arrow (first,(***))
import Test.QuickCheck (Arbitrary(..),frequency)

data ModeI m r
  = Square  m (ModeI m r)
  | Diamond m (ModeI m r)
  | LiftMode r
  deriving (Eq,Ord,Show)

instance (Arbitrary m, Arbitrary r) => Arbitrary (ModeI m r) where
  arbitrary = frequency
    [ ( 1 , Square   <$> arbitrary <*> arbitrary )
    , ( 1 , Diamond  <$> arbitrary <*> arbitrary )
    , ( 2 , LiftMode <$> arbitrary               )
    ]
  shrink = \case
    Square   m p -> [Square  n q | n <- shrink m, q <- shrink p]
    Diamond  m p -> [Diamond n q | n <- shrink m, q <- shrink p]
    LiftMode   p -> [LiftMode  q | q <- shrink p]

instance Atomic a r => Atomic a (ModeI m r) where
  atom = LiftMode . atom

instance Boolean r => Boolean (ModeI m r) where
  tt = LiftMode tt
  ff = LiftMode ff

instance Modal m r => Embed r (ModeI m r) where
  embed = LiftMode
  lower = \case
    Square  m p -> square  m $ lower p
    Diamond m p -> diamond m $ lower p
    LiftMode  p -> p

instance (Modal m r, EmbedStar a r) => EmbedStar a (ModeI m r)

instance Contextual r (ModeI m r) where
  type Context (ModeI m r) = (,) [(m,Bool)]
  embedCxt (ms,p) = foldr (uncurry wrapMode) (LiftMode p) ms
    where
    wrapMode m = Square m ? Diamond m
  lowerCxt = \case
    Square  m p -> first ((m,True ):) $ lowerCxt p
    Diamond m p -> first ((m,False):) $ lowerCxt p
    LiftMode  p -> ([],p)

instance ContextStar a r => ContextStar a (ModeI m r) where
  type AllContext (ModeI m r) = CompAllCxt r (ModeI m r)

instance Modal m r => Propositional (ModeI m r) where
  neg    = mapCxt $ map (flipMode *** not) *** neg
  (.|.)  = placeCxt2_   (.|.)
  (.&.)  = placeCxt2_   (.&.)
  (.^.)  = placeCxt2_   (.^.)
  (.->.) = distribCxt2_ (.->.)
  (.==.) = placeCxt2_   (.==.)

instance Modal m r => Modal m (ModeI m r) where
  square  = Square
  diamond = Diamond

instance (Modal m r, Modal n r) => Modal n (ModeI m r) where
  square  = placeCxt1_ . square
  diamond = placeCxt1_ . diamond

