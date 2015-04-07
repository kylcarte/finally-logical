{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Logic.CNF where

import Data.Logic.Atomic.Class
import Data.Logic.Boolean.Class
import Data.Logic.Decidable
import Data.Logic.Embed
import Data.Logic.Propositional.Class
import Data.Logic.Modal.Class
import Data.Logic.Render

import Test.QuickCheck (CoArbitrary(..), Arbitrary(..))

import Control.Applicative

type StrProp r = (Atomic String r, Propositional r)

-- PushNeg {{{

newtype PushNeg r = PushNeg
  { pushNeg :: Bool -> r
  }

pushNeg_ :: PushNeg r -> r
pushNeg_ = ($ False) . pushNeg

instance Arbitrary r => Arbitrary (PushNeg r) where
  arbitrary = PushNeg <$> arbitrary
  shrink p  = [PushNeg q | q <- shrink $ pushNeg p]

instance Propositional r => Embed r (PushNeg r) where
  embed p = PushNeg $ neg p ? p
  lower p = pushNeg p False

instance (Propositional r, EmbedStar a r) => EmbedStar a (PushNeg r) where
  embedStar = embed . embedStar
  lowerStar = lowerStar . lower

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
  neg   = mapCxt (. not)
  (.|.) = distrib2 $ (.&.) ? (.|.)
  (.&.) = distrib2 $ (.|.) ? (.&.)
  (.^.) = distrib2_ (.^.)

instance (Modal r, Propositional r) => Modal (PushNeg r) where
  square  = distrib1 $ diamond ? square
  diamond = distrib1 $ square  ? diamond

-- }}}

-- PushDisj {{{

newtype PushDisj r = PushDisj
  { pushDisj :: Maybe r -> r
  }

pushDisj_ :: PushDisj r -> r
pushDisj_ = ($ Nothing) . pushDisj

instance (Arbitrary r, CoArbitrary r) => Arbitrary (PushDisj r) where
  arbitrary = PushDisj <$> arbitrary

instance Propositional r => Embed r (PushDisj r) where
  embed p = PushDisj $ maybe p (p .|.)
  lower p = pushDisj p Nothing

instance (Propositional r, EmbedStar a r) => EmbedStar a (PushDisj r) where
  embedStar = embed . embedStar
  lowerStar = lowerStar . lower

instance Propositional r => Contextual r (PushDisj r) where
  type Context (PushDisj r) = (->) (Maybe r)
  embedCxt = PushDisj
  lowerCxt = pushDisj

instance (Atomic a r, Propositional r) => Atomic a (PushDisj r) where
  atom = embed . atom

instance (Boolean r, Propositional r) => Boolean (PushDisj r) where
  tt = embed tt
  ff = embed ff

instance Propositional r => Propositional (PushDisj r) where
  neg   = place1_ neg
  (.|.) = apChainCxtR
  (.&.) = distrib2_ (.&.)

-- }}}

-- PushConj {{{

newtype PushConj r = PushConj
  { pushConj :: Maybe r -> r
  }

pushConj_ :: PushConj r -> r
pushConj_ = ($ Nothing) . pushConj

instance (Arbitrary r, CoArbitrary r) => Arbitrary (PushConj r) where
  arbitrary = PushConj <$> arbitrary

instance Propositional r => Embed r (PushConj r) where
  embed p = PushConj $ maybe p (p .&.)
  lower p = pushConj p Nothing

instance (Propositional r, EmbedStar a r) => EmbedStar a (PushConj r) where
  embedStar = embed . embedStar
  lowerStar = lowerStar . lower

instance Propositional r => Contextual r (PushConj r) where
  type Context (PushConj r) = (->) (Maybe r)
  embedCxt = PushConj
  lowerCxt = pushConj

instance (Atomic a r, Propositional r) => Atomic a (PushConj r) where
  atom = embed . atom

instance (Boolean r, Propositional r) => Boolean (PushConj r) where
  tt = embed tt
  ff = embed ff

instance Propositional r => Propositional (PushConj r) where
  neg   = place1_ neg
  (.|.) = apChainCxtR
  (.&.) = distrib2_ (.&.)

-- }}}

p0 :: StrProp r => r
p0 = neg (atom "A" .|. atom "B")

p1 :: StrProp r => r
p1 = atom "C" .|. (atom "A" .&. atom "B")

p2 :: StrProp r => r
p2 = (atom "A" .&. atom "B") .|. atom "C"

p3 :: StrProp r => r
p3 = (atom "A" .|. atom "B") .|. atom "C"

p4 :: StrProp r => r
p4 = ((atom "A" .|. atom "B") .|. atom "C") .|. ((atom "D" .|. atom "E") .|. atom "F") .|. atom "G"

testProp :: PushNeg (PushDisj (PushConj Render)) -> IO ()
testProp = renderIO . pushConj_ . pushDisj_ . pushNeg_

