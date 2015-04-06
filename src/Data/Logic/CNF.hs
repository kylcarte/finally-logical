{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Logic.CNF where

import Data.Logic.Propositional.Class
import Data.Logic.Modal.Class
import Data.Logic.Atomic
import Data.Logic.Boolean
import Data.Logic.Decidable
import Data.Logic.Embed

import Test.QuickCheck (Arbitrary)

import GHC.Generics

-- PushNeg {{{

newtype PushNeg r = PushNeg
  { pushNeg :: Bool -> r
  } deriving (Generic)

instance Propositional r => Embed r (PushNeg r) where
  embed p = PushNeg $ neg p ? p
  lower p = pushNeg p False

instance Propositional r => Contextual r (PushNeg r) where
  type Context (PushNeg r) = (->) Bool
  embedCxt = PushNeg
  lowerCxt = pushNeg

instance (Atomic a r, Propositional r) => Atomic a (PushNeg r) where
  atom = embed . atom

instance (Boolean r, Propositional r) => Boolean (PushNeg r) where
  tt = embed tt
  ff = embed ff

instance Propositional r => Propositional (PushNeg r) where
  neg   = mapLower neg
  (.|.) = distrib2 $ (.&.) ? (.|.)
  (.&.) = distrib2 $ (.|.) ? (.&.)
  (.^.) = distrib2_ (.^.)

instance (Modal r, Propositional r) => Modal (PushNeg r) where
  square  = distrib1 $ diamond ? square
  diamond = distrib1 $ square  ? diamond

instance Arbitrary r => Arbitrary (PushNeg r)

-- }}}

