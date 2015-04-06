{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Data.Logic.Boolean where

import Control.Applicative
import Data.Functor.Identity

class Boolean r where
  tt :: r
  ff :: r
  fromBool :: Bool -> r
  fromBool = \case
    True -> tt
    _    -> ff

instance Boolean Bool where
  tt = True
  ff = False

instance Boolean r => Boolean (Identity r) where
  tt = pure tt
  ff = pure ff

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

data AnyBoolean = AnyBoolean
 { getBoolean :: forall r. Boolean r => r
 }

instance Boolean AnyBoolean where
  tt = AnyBoolean tt
  ff = AnyBoolean ff

