{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Data.Logic.Modal.Class where

import Data.Logic.Decidable
import Data.Logic.Propositional.Class
import Data.Logic.Util

import Test.QuickCheck (Arbitrary(..))

class (IsMode m, Propositional r) => Modal m r where
  square  :: m -> r -> r
  diamond :: m -> r -> r
  ----
  square  m p = neg $ diamond m $ neg p
  diamond m p = neg $ square  m $ neg p
  {-# MINIMAL square | diamond #-}

class IsMode m where
  flipMode :: m -> m
  flipMode = id
  showsMode :: m -> Bool -> ShowS
  default showsMode :: Show m => m -> Bool -> ShowS
  showsMode m b =
      showStringChoice "◻" "⋄" b
    . showString ("{" ++ show m ++ "} ")

-- Default Defs {{{

defShowsSquare :: Show m => m -> ShowS
defShowsSquare m = showString $ "◻{" ++ show m ++ "} "

defShowsDiamond :: Show m => m -> ShowS
defShowsDiamond m = showString $ "⋄{" ++ show m ++ "} "

-- }}}

-- Util Ops {{{

showStringChoice :: String -> String -> Bool -> ShowS
showStringChoice t f = showString . (t ? f)

showsSquare :: IsMode m => m -> ShowS
showsSquare = flip showsMode True

showsDiamond :: IsMode m => m -> ShowS
showsDiamond = flip showsMode False

-- }}}

-- Alethic {{{

data Alethic = Alethic deriving (Eq,Ord,Show)

instance Arbitrary Alethic where
  arbitrary = pure Alethic

instance IsMode Alethic where
  showsMode _ = showStringChoice "Necessarily " "Possibly "

nec :: Modal Alethic r => r -> r
nec = square Alethic

poss :: Modal Alethic r => r -> r
poss = diamond Alethic

-- }}}

-- Deontic {{{

data Deontic = Deontic deriving (Eq,Ord,Show)

instance Arbitrary Deontic where
  arbitrary = pure Deontic

instance IsMode Deontic where
  showsMode _ = showStringChoice "Obligatorily " "Permissibly "

oblig :: Modal Deontic r => r -> r
oblig = square Deontic

permis :: Modal Deontic r => r -> r
permis = diamond Deontic

-- }}}

