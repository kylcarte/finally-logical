{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Logic.Embed where

import Control.Applicative

class Embed r s | s -> r where
  lower :: s -> r
  embed :: r -> s

class Embed r r => EmbedStar r s | s -> r where
  embedStar :: r -> s
  lowerStar :: s -> r
  default lowerStar :: (EmbedStar r t, Embed t s) => s -> r
  lowerStar = lowerStar . lower
  default embedStar :: (EmbedStar r t, Embed t s) => r -> s
  embedStar = embed . embedStar

instance Embed Bool Bool where
  lower = id
  embed = id

instance EmbedStar Bool Bool where
  lowerStar = id
  embedStar = id

class (Embed r s, Applicative (Context s)) => Contextual r s | s -> r where
  type Context s :: * -> *
  embedCxt :: Context s r -> s
  lowerCxt :: s -> Context s r

place1 :: Contextual r s => Context s (r -> r) -> s -> s
place1 op p = embedCxt $ op <&> lower p

place1_ :: Contextual r s => (r -> r) -> s -> s
place1_ = place1 . pure

place2 :: Contextual r s => Context s (r -> r -> r) -> s -> s -> s
place2 op p q = embedCxt $ op <&> lower p <&> lower q

place2_ :: Contextual r s => (r -> r -> r) -> s -> s -> s
place2_ = place2 . pure

distrib1 :: Contextual r s => Context s (r -> r) -> s -> s
distrib1 op p = embedCxt $ op <*> lowerCxt p

distrib1_ :: Contextual r s => (r -> r) -> s -> s
distrib1_ = distrib1 . pure

distrib2 :: Contextual r s => Context s (r -> r -> r) -> s -> s -> s
distrib2 op p q = embedCxt $ op <*> lowerCxt p <*> lowerCxt q

distrib2_ :: Contextual r s => (r -> r -> r) -> s -> s -> s
distrib2_ = distrib2 . pure

mapCxt :: (Contextual r s, Contextual t u) => (Context s r -> Context u t) -> s -> u
mapCxt f = embedCxt . f . lowerCxt

mapLower :: (Contextual r s, Contextual t u, Context s ~ Context u) => (r -> t) -> s -> u
mapLower f = embedCxt . fmap f . lowerCxt

{-
usingCxt2 :: Contextual r s => (r -> Context s r) -> s -> s -> s
usingCxt2 f p q = 
-}

(<&>) :: Functor f => f (a -> b) -> a -> f b
mf <&> a = fmap ($ a) mf
infixl 4 <&>

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 1 .:

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = f x `op` f y

