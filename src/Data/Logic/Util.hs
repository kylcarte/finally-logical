{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Logic.Util
  ( module Data.Logic.Util
  , module Exports
  ) where

import Data.Function         as Exports (on)
import Control.Applicative   as Exports
import Data.Functor.Identity as Exports
import Data.Functor.Compose  as Exports

(<&>) :: Functor f => f (a -> b) -> a -> f b
f <&> a = ($ a) <$> f
infixl 4 <&>

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 1 .:

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
infixr 1 .>

_C :: f (g a) -> Compose f g a
_C = Compose

_c :: Compose f g a -> f (g a)
_c = getCompose

type (:.:) = Compose
infixr 4 :.:

type f :$: a = f a
infixr 3 :$:

