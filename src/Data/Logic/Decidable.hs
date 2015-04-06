{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Logic.Decidable where

import Data.Logic.Atomic

import Control.Applicative
import Data.Functor.Identity
import Data.Functor.Compose

class Applicative (Decide r) => Decidable r where
  type Decide r :: * -> *
  type Decide r = Identity
  truth :: r -> Decide r Bool

type Decidable_ r  = (Decidable r  , Decide r ~ Identity)

-- Decidable Ops {{{

truth_ :: Decidable_ r => r -> Bool
truth_ = runIdentity . truth

if' :: Decidable r => r -> a -> a -> Decide r a
if' a t f = (\b -> if b then t else f) <$> truth a

if_ :: Decidable_ r => r -> a -> a -> a
if_ a t f = runIdentity $ if' a t f

cond :: Decidable r => a -> a -> r -> Decide r a
cond t f a = if' a t f

cond_ :: Decidable_ r => a -> a -> r -> a
cond_ t f = runIdentity . cond t f

(.?.) :: Decidable_ r => a -> a -> r -> a
(.?.) = cond_
infix 5 .?.

-- }}}

-- Default Defs {{{

defTruth1 :: (Decidable a, Applicative f) => f a -> (f `Compose` Decide a) Bool
defTruth1 = Compose . fmap truth

defTruthConj :: (Decidable r, Decidable s)
  => r -> s -> (Decide r `Compose` Decide s) Bool
defTruthConj r s = Compose $ ($ truth s) <$> fmap . (&&) <$> truth r

defTruthConj' :: (Decidable r, Decidable s, Decide r ~ Decide s)
  => r -> s -> Decide r Bool
defTruthConj' r s = (&&) <$> truth r <*> truth s

-- }}}

-- Decidable instances {{{

instance Decidable Bool where
  truth = pure

instance Decidable (Atom a) where
  type Decide (Atom a) = ((->) (a -> Bool))
  truth (Atom a) pr = pr a

instance Decidable r => Decidable (Identity r) where
  type Decide (Identity r) = Decide r
  truth = truth . runIdentity

instance Decidable r => Decidable (Maybe r) where
  type Decide (Maybe r) = Maybe `Compose` Decide r
  truth = defTruth1

instance Decidable r => Decidable [r] where
  type Decide [r] = [] `Compose` Decide r
  truth = defTruth1

instance Decidable r => Decidable (Either e r) where
  type Decide (Either e r) = Either e `Compose` Decide r
  truth = defTruth1

instance Decidable r => Decidable (a -> r) where
  type Decide (a -> r) = ((->) a) `Compose` Decide r
  truth = defTruth1

instance Decidable r => Decidable (IO r) where
  type Decide (IO r) = IO `Compose` Decide r
  truth = defTruth1

instance
  ( Decidable r
  , Decidable s
  ) => Decidable (r,s) where
  type Decide (r,s) = Decide r `Compose` Decide s
  truth (r,s) = defTruthConj r s

instance
  ( Decidable r
  , Decidable s
  , Decidable t
  ) => Decidable (r,s,t) where
  type Decide (r,s,t) = Decide r `Compose` Decide (s,t)
  truth (r,s,t) = defTruthConj r (s,t)

instance
  ( Decidable r
  , Decidable s
  , Decidable t
  , Decidable u
  ) => Decidable (r,s,t,u) where
  type Decide (r,s,t,u) = Decide r `Compose` Decide (s,t,u)
  truth (r,s,t,u) = defTruthConj r (s,t,u)

instance
  ( Decidable r
  , Decidable s
  , Decidable t
  , Decidable u
  , Decidable v
  ) => Decidable (r,s,t,u,v) where
  type Decide (r,s,t,u,v) = Decide r `Compose` Decide (s,t,u,v)
  truth (r,s,t,u,v) = defTruthConj r (s,t,u,v)

-- }}}

