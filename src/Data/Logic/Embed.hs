{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Logic.Embed where

import Data.Logic.Util

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

overLower :: Embed r s => (r -> r) -> s -> s
overLower f = embed . f . lower

overLower2With :: Embed r s => (r -> r) -> (r -> r -> r) -> s -> s -> s
overLower2With f op p q = embed $ f $ lower p `op` lower q

overLower2 :: Embed r s => (r -> r -> r) -> s -> s -> s
overLower2 = overLower2With id

class Applicative (Context s) => Contextual r s | s -> r where
  type Context s :: * -> *
  embedCxt :: Context s r -> s
  lowerCxt :: s -> Context s r

class
  ( Contextual r r
  , Context r ~ Identity
  , Applicative (AllContext s)
  ) => ContextStar r s | s -> r where
  type AllContext s :: * -> *
  embedCxtStar :: AllContext s r -> s
  lowerCxtStar :: s -> AllContext s r
  default embedCxtStar :: ComposedCxtStar r t s => AllContext s r -> s
  embedCxtStar = compEmbedCxtStar
  default lowerCxtStar :: ComposedCxtStar r t s => s -> AllContext s r
  lowerCxtStar = compLowerCxtStar

instance Contextual Bool Bool where
  type Context Bool = Identity
  embedCxt = runIdentity
  lowerCxt = Identity

instance ContextStar Bool Bool where
  type AllContext Bool = Identity
  embedCxtStar = embedCxt
  lowerCxtStar = lowerCxt

type CompAllCxt b c = Compose (Context c) (AllContext b)

type ComposedCxtStar a b c =
  ( ContextStar a b
  , Contextual  b c
  , AllContext c ~ CompAllCxt b c
  )

compEmbedCxtStar :: forall a b c. (ContextStar a b, Contextual b c) =>
  Compose (Context c) (AllContext b) a -> c
compEmbedCxtStar = embedCxt . fmap (embedCxtStar :: AllContext b a -> b) . _c

compLowerCxtStar :: forall a b c. (ContextStar a b, Contextual b c) =>
  c -> Compose (Context c) (AllContext b) a
compLowerCxtStar = _C . fmap lowerCxtStar . lowerCxt

type EmbedCxt r s = (Embed r s, Contextual r s)

placeCxt1 :: EmbedCxt r s => Context s (r -> r) -> s -> s
placeCxt1 f p = embedCxt $ f <&> lower p

placeCxt1_ :: EmbedCxt r s => (r -> r) -> s -> s
placeCxt1_ = placeCxt1 . pure

placeCxt2 :: EmbedCxt r s => Context s (r -> r -> r) -> s -> s -> s
placeCxt2 f p q = embedCxt $ f <&> lower p <&> lower q

placeCxt2_ :: EmbedCxt r s => (r -> r -> r) -> s -> s -> s
placeCxt2_ = placeCxt2 . pure

distribCxt1 :: Contextual r s => Context s (r -> r) -> s -> s
distribCxt1 f p = embedCxt $ f <*> lowerCxt p

distribCxt1_ :: Contextual r s => (r -> r) -> s -> s
distribCxt1_ = distribCxt1 . pure

distribCxt2 :: Contextual r s => Context s (r -> r -> r) -> s -> s -> s
distribCxt2 f p q = embedCxt $ f <*> lowerCxt p <*> lowerCxt q

distribCxt2_ :: Contextual r s => (r -> r -> r) -> s -> s -> s
distribCxt2_ = distribCxt2 . pure

mapCxt :: (Contextual r s, Contextual t u) => (Context s r -> Context u t) -> s -> u
mapCxt f = embedCxt . f . lowerCxt

mapLower :: (Contextual r s, Contextual t u, Context s ~ Context u) => (r -> t) -> s -> u
mapLower f = embedCxt . fmap f . lowerCxt

apCxt :: EmbedCxt r s => (r -> Context s (r -> r)) -> s -> s -> s
apCxt f p q = embedCxt $ f (lower q) <*> lowerCxt p

-- blarg ?
chainCxtR :: (Contextual r s, Context s ~ (->) t)
  => (r -> t) -> s -> s -> s
chainCxtR f p q = embedCxt $ lowerCxt p . f . lowerCxt q

chainCxtL :: (Contextual r s, Context s ~ (->) t)
  => (r -> t) -> s -> s -> s
chainCxtL f p q = embedCxt $ lowerCxt q . f . lowerCxt p

apChainCxtR :: (Contextual r s, Context s ~ (->) (f r), Applicative f) => s -> s -> s
apChainCxtR = chainCxtR pure

apChainCxtL :: (Contextual r s, Context s ~ (->) (f r), Applicative f) => s -> s -> s
apChainCxtL = chainCxtL pure

