{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.Logic.Boolean.Class where

import Control.Applicative
import Data.Functor.Identity
import Data.Functor.Compose

class Boolean r where
  tt :: r
  ff :: r
  fromBool :: Bool -> r
  fromBool b = if b then tt else ff

data AnyBoolean = AnyBoolean
 { getBoolean :: forall r. Boolean r => r
 }

instance Boolean AnyBoolean where
  tt = AnyBoolean tt
  ff = AnyBoolean ff

-- Boolean instances {{{

instance Boolean Bool where
  tt = True
  ff = False

instance Boolean r => Boolean (Identity r) where
  tt = pure tt
  ff = pure ff

instance Boolean (f (g r)) => Boolean (Compose f g r) where
  tt = Compose tt
  ff = Compose ff

instance Boolean r => Boolean (Maybe r) where
  tt = pure tt
  ff = pure ff

instance Boolean r => Boolean [r] where
  tt = pure tt
  ff = pure ff

instance Boolean r => Boolean (Either a r) where
  tt = pure tt
  ff = pure ff

instance Boolean r => Boolean (a -> r) where
  tt = pure tt
  ff = pure ff

instance Boolean r => Boolean (IO r) where
  tt = pure tt
  ff = pure ff

instance
  ( Boolean r
  , Boolean s
  ) => Boolean (r,s) where
  tt = ( tt , tt )
  ff = ( ff , ff )

instance
  ( Boolean r
  , Boolean s
  , Boolean t
  ) => Boolean (r,s,t) where
  tt = ( tt , tt , tt )
  ff = ( ff , ff , ff )

instance
  ( Boolean r
  , Boolean s
  , Boolean t
  , Boolean u
  ) => Boolean (r,s,t,u) where
  tt = ( tt , tt , tt , tt )
  ff = ( ff , ff , ff , ff )

instance
  ( Boolean r
  , Boolean s
  , Boolean t
  , Boolean u
  , Boolean v
  ) => Boolean (r,s,t,u,v) where
  tt = ( tt , tt , tt , tt , tt )
  ff = ( ff , ff , ff , ff , ff )

-- }}}

