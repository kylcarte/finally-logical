{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Logic.Substitution where

-- import Control.Lens

import Prelude hiding (map)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid

newtype Subst l a b = Subst
  { unSubst :: Map a (l b)
  } deriving (Eq,Ord,Show)

type instance Index   (Subst l a b) = a
type instance IxValue (Subst l a b) = l b

type Subst' l a = Subst l a a

makeLensesFor
  [ ( "unSubst" , "_unSubst" )
  ] ''Subst

_Subst :: Iso (Map a (l b)) (Map c (m d)) (Subst l a b) (Subst m c d)
_Subst = from _unSubst

instance Ord a => Ixed (Subst l a b) where
  ix i = _unSubst . ix i

instance Ord a => At (Subst l a b) where
  at i = _unSubst . at i

map :: (Functor l, Ord c) => (a -> c) -> (b -> d) -> Subst l a b -> Subst l c d
map f g = _unSubst
  %~ M.mapKeys f
   . fmap (fmap g)

substWith :: (Monad l, Ord a) => (a -> l b) -> Subst l a b -> l a -> l b
substWith f s p = p >>= \a -> fromMaybe (f a) $ s ^. at a

subst :: (Monad l, Ord a) => Subst' l a -> l a -> l a
subst = substWith return

composeWith :: (Monad l, Ord b) => (b -> l c) -> Subst l b c -> Subst l a b -> Subst l a c
composeWith f = over (_unSubst.mapped) . substWith f

compose :: (Monad l, Ord a) => Subst' l a -> Subst' l a -> Subst' l a
compose = composeWith return

instance (Monad l, Ord a, a ~ b) => Monoid (Subst l a b) where
  mempty  = Subst mempty
  mappend = compose

