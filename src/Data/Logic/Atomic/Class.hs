{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Logic.Atomic.Class where

import Data.Functor.Identity
import Data.Functor.Compose

class Atomic a r | r -> a where
  atom   :: a -> r

data AnyAtomic a = AnyAtomic
  { getAtomic :: forall r. Atomic a r => r
  }

instance Atomic a (AnyAtomic a) where
  atom a = AnyAtomic $ atom a

-- Atomic instances {{{

instance Atomic Bool Bool where
  atom = id

instance Atomic String String where
  atom = id

instance Atomic a r => Atomic a (Identity r) where
  atom = return . atom

instance Atomic a (f (g r)) => Atomic a (Compose f g r) where
  atom = Compose . atom

instance Atomic a r => Atomic a (Maybe r) where
  atom = return . atom

instance Atomic a r => Atomic a [r] where
  atom = return . atom

instance Atomic a r => Atomic a (Either e r) where
  atom = return . atom

instance Atomic a r => Atomic a (b -> r) where
  atom = return . atom

instance Atomic a r => Atomic a (IO r) where
  atom = return . atom

instance
  ( Atomic a r
  , Atomic a s
  ) => Atomic a (r,s) where
  atom a = ( atom a , atom a )

instance
  ( Atomic a r
  , Atomic a s
  , Atomic a t
  ) => Atomic a (r,s,t) where
  atom a = ( atom a , atom a , atom a )

instance
  ( Atomic a r
  , Atomic a s
  , Atomic a t
  , Atomic a u
  ) => Atomic a (r,s,t,u) where
  atom a = ( atom a , atom a , atom a , atom a)

instance
  ( Atomic a r
  , Atomic a s
  , Atomic a t
  , Atomic a u
  , Atomic a v
  ) => Atomic a (r,s,t,u,v) where
  atom a = ( atom a , atom a , atom a , atom a , atom a )

-- }}}

