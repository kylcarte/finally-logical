
module Data.Logic.Modal.Class where

import Data.Logic.Propositional.Class

class Propositional r => Modal r where
  square  :: r -> r
  diamond :: r -> r
  ----
  square  p = neg $ diamond $ neg p
  diamond p = neg $ square  $ neg p
  {-# MINIMAL square | diamond #-}

