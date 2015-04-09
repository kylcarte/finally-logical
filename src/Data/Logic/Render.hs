{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Logic.Render where

import Data.Logic.Atomic.Class
import Data.Logic.Boolean.Class
import Data.Logic.Embed
import Data.Logic.Propositional.Class
import Data.Logic.Modal.Class
import Data.Logic.Util

newtype Render = Render
  { rendersPrec :: Int -> ShowS
  }

instance Show Render where
  showsPrec d = flip rendersPrec d

instance Embed Render Render where
  lower = id
  embed = id

instance EmbedStar Render Render where
  lowerStar = id
  embedStar = id

instance Contextual Render Render where
  type Context Render = Identity
  lowerCxt = Identity
  embedCxt = runIdentity

instance ContextStar Render Render where
  type AllContext Render = Identity
  lowerCxtStar = lowerCxt
  embedCxtStar = embedCxt

-- Util Ops {{{

renderIO :: Render -> IO ()
renderIO = putStrLn . render

render :: Render -> String
render = ($ "") . renders

renders :: Render -> ShowS
renders = ($ 0) . rendersPrec

renderString :: String -> Render
renderString s = Render $ \_ -> showString s

renderChar :: Char -> Render
renderChar c = Render $ \_ -> showChar c

-- }}}

instance Atomic String Render where
  atom   = renderString

instance Boolean Render where
  tt = Render $ \_ -> showString "True"
  ff = Render $ \_ -> showString "False"

instance Propositional Render where
  neg p = Render $ \d -> showParen (d > 10)
    $ showString "¬ "
    . rendersPrec p 11
  p .|. q = Render $ \d -> showParen (d > 3)
    $ rendersPrec p 4
    . showString " ∨ "
    . rendersPrec q 3
  p .^. q = Render $ \d -> showParen (d > 3)
    $ rendersPrec p 4
    . showString " ⊕ "
    . rendersPrec q 3
  p .&. q = Render $ \d -> showParen (d > 4)
    $ rendersPrec p 5
    . showString " ∧ "
    . rendersPrec q 4
  p .->. q = Render $ \d -> showParen (d > 2)
    $ rendersPrec p 3
    . showString " ⇒ "
    . rendersPrec q 2
  p .==. q = Render $ \d -> showParen (d > 1)
    $ rendersPrec p 2
    . showString " ⇔ "
    . rendersPrec q 1

instance IsMode m => Modal m Render where
  square m p = Render $ \d -> showParen (d > 10)
    $ showsSquare m
    . rendersPrec p 11
  diamond m p = Render $ \d -> showParen (d > 10)
    $ showsDiamond m
    . rendersPrec p 11

