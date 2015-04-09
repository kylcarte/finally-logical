{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Test where

import Data.Logic.Atomic
import Data.Logic.Boolean
import Data.Logic.Propositional
import Data.Logic.Modal

import Data.Logic.Embed
import Data.Logic.Render
import Data.Logic.Util

import Control.Monad (replicateM_)
import Test.QuickCheck hiding ((.&.))

newtype P = P { prop :: Render }

instance Show P where
  showsPrec d = showsPrec d . prop

instance Arbitrary P where
  arbitrary = P . renderString <$> propNames

baseNames :: Gen String
baseNames = elements $ map return ['A'..'Z']

propNames :: Gen String
propNames = frequency
    [ ( 26 , baseNames )
    , ( 1  , (++) <$> baseNames <*> propNames )
    ]

instance Embed Render P where
  embed = P
  lower = prop

instance EmbedStar Render P where
  embedStar = embed
  lowerStar = lower

instance Contextual Render P where
  type Context P = Identity
  embedCxt = P . runIdentity
  lowerCxt = Identity . prop

instance ContextStar Render P where
  type AllContext P = Identity
  embedCxtStar = embed . embedCxtStar
  lowerCxtStar = lowerCxtStar . lower

instance Atomic String P where
  atom = embed . renderString

instance Boolean P where
  tt = embed tt
  ff = embed ff

instance Propositional P where
  neg    = mapLower neg
  (.|.)  = overLower2 (.|.)
  (.^.)  = overLower2 (.^.)
  (.&.)  = overLower2 (.&.)
  (.->.) = overLower2 (.->.)
  (.==.) = overLower2 (.==.)

instance IsMode m => Modal m P where
  square  = mapLower . square
  diamond = mapLower . diamond

type Logic = ModeI Alethic (PropI (BoolI P))

printGen :: Int -> IO ()
printGen = flip replicateM_ $ generate (arbitrary :: Gen Logic) >>= putStrLn . renderLogic

renderLogic :: Logic -> String
renderLogic = render . lowerStar

