{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Logic.Atomic where

class Atomic a r | r -> a where
  atom   :: a -> r

newtype Atom a = Atom a
  deriving (Eq,Ord,Show)

instance Atomic a (Atom a) where
  atom = Atom

