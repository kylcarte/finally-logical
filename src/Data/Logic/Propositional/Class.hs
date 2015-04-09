{-# LANGUAGE FlexibleContexts #-}

module Data.Logic.Propositional.Class where

import Data.Logic.Util

class Propositional r where
  neg    :: r -> r
  (.|.)  :: r -> r -> r
  (.&.)  :: r -> r -> r
  (.^.)  :: r -> r -> r
  (.->.) :: r -> r -> r
  (.==.) :: r -> r -> r
  --------------------
  (.|.)  = defDisj
  (.&.)  = defConj
  (.^.)  = defExDisj
  (.->.) = defImpl
  (.==.) = defEqv
  {-# MINIMAL neg, ((.|.) | (.&.)) #-}
infixr 3 .|.
infixr 3 .^.
infixr 4 .&.
infixr 2 .->.
infixr 1 .==.

-- Default Defs {{{

defDisj :: Propositional r => r -> r -> r
defDisj p q = neg $ neg p .&. neg q

defExDisj :: Propositional r => r -> r -> r
defExDisj p q = (p .|. q) .&. neg (p .&. q)

defConj :: Propositional r => r -> r -> r
defConj p q = neg $ neg p .|. neg q

defImpl :: Propositional r => r -> r -> r
defImpl p q = neg p .|. q

defEqv :: Propositional r => r -> r -> r
defEqv p q = (p .->. q) .&. (q .->. p)

defNeg1 :: (Functor f, Propositional a) => f a -> f a
defNeg1 = fmap neg

defDisj1 :: (Applicative f, Propositional a) => f a -> f a -> f a
defDisj1 = liftA2 (.|.)

defExDisj1 :: (Applicative f, Propositional a) => f a -> f a -> f a
defExDisj1 = liftA2 (.^.)

defConj1 :: (Applicative f, Propositional a) => f a -> f a -> f a
defConj1 = liftA2 (.&.)

defImpl1 :: (Applicative f, Propositional a) => f a -> f a -> f a
defImpl1 = liftA2 (.->.)

defEqv1 :: (Applicative f, Propositional a) => f a -> f a -> f a
defEqv1 = liftA2 (.==.)

-- }}}

-- Propositional instances {{{

instance Propositional Bool where
  neg   = not
  (.|.) = (||)
  (.&.) = (&&)

instance Propositional r => Propositional (Identity r) where
  neg    = defNeg1
  (.|.)  = defDisj1
  (.^.)  = defExDisj1
  (.&.)  = defConj1
  (.->.) = defImpl1
  (.==.) = defEqv1

instance Propositional (f (g r)) => Propositional (Compose f g r) where
  neg    = Compose . neg . getCompose
  (.|.)  = Compose .: (.|.)  `on` getCompose
  (.&.)  = Compose .: (.&.)  `on` getCompose
  (.^.)  = Compose .: (.^.)  `on` getCompose
  (.->.) = Compose .: (.->.) `on` getCompose
  (.==.) = Compose .: (.==.) `on` getCompose

instance Propositional r => Propositional (Maybe r) where
  neg    = defNeg1
  (.|.)  = defDisj1
  (.^.)  = defExDisj1
  (.&.)  = defConj1
  (.->.) = defImpl1
  (.==.) = defEqv1

instance Propositional r => Propositional [r] where
  neg    = defNeg1
  (.|.)  = defDisj1
  (.^.)  = defExDisj1
  (.&.)  = defConj1
  (.->.) = defImpl1
  (.==.) = defEqv1

instance Propositional r => Propositional (Either a r) where
  neg    = defNeg1
  (.|.)  = defDisj1
  (.^.)  = defExDisj1
  (.&.)  = defConj1
  (.->.) = defImpl1
  (.==.) = defEqv1

instance Propositional r => Propositional (a -> r) where
  neg    = defNeg1
  (.|.)  = defDisj1
  (.^.)  = defExDisj1
  (.&.)  = defConj1
  (.->.) = defImpl1
  (.==.) = defEqv1

instance Propositional r => Propositional (IO r) where
  neg    = defNeg1
  (.|.)  = defDisj1
  (.^.)  = defExDisj1
  (.&.)  = defConj1
  (.->.) = defImpl1
  (.==.) = defEqv1

instance
  ( Propositional r
  , Propositional s
  ) => Propositional (r,s) where
  neg (r,s)            = ( neg r      , neg s      )
  (r1,s1) .|.  (r2,s2) = ( r1 .|.  r2 , s1 .|.  s2 )
  (r1,s1) .^.  (r2,s2) = ( r1 .^.  r2 , s1 .^.  s2 )
  (r1,s1) .&.  (r2,s2) = ( r1 .&.  r2 , s1 .&.  s2 )
  (r1,s1) .->. (r2,s2) = ( r1 .->. r2 , s1 .->. s2 )
  (r1,s1) .==. (r2,s2) = ( r1 .==. r2 , s1 .==. s2 )

instance
  ( Propositional r
  , Propositional s
  , Propositional t
  ) => Propositional (r,s,t) where
  neg (r,s,t)                = ( neg r      , neg s      , neg t      )
  (r1,s1,t1) .|.  (r2,s2,t2) = ( r1 .|.  r2 , s1 .|.  s2 , t1 .|.  t2 )
  (r1,s1,t1) .^.  (r2,s2,t2) = ( r1 .^.  r2 , s1 .^.  s2 , t1 .^.  t2 )
  (r1,s1,t1) .&.  (r2,s2,t2) = ( r1 .&.  r2 , s1 .&.  s2 , t1 .&.  t2 )
  (r1,s1,t1) .->. (r2,s2,t2) = ( r1 .->. r2 , s1 .->. s2 , t1 .->. t2 )
  (r1,s1,t1) .==. (r2,s2,t2) = ( r1 .==. r2 , s1 .==. s2 , t1 .==. t2 )

instance
  ( Propositional r
  , Propositional s
  , Propositional t
  , Propositional u
  ) => Propositional (r,s,t,u) where
  neg (r,s,t,u)                    = ( neg r      , neg s      , neg t      , neg u      )
  (r1,s1,t1,u1) .|.  (r2,s2,t2,u2) = ( r1 .|.  r2 , s1 .|.  s2 , t1 .|.  t2 , u1 .|.  u2 )
  (r1,s1,t1,u1) .^.  (r2,s2,t2,u2) = ( r1 .^.  r2 , s1 .^.  s2 , t1 .^.  t2 , u1 .^.  u2 )
  (r1,s1,t1,u1) .&.  (r2,s2,t2,u2) = ( r1 .&.  r2 , s1 .&.  s2 , t1 .&.  t2 , u1 .&.  u2 )
  (r1,s1,t1,u1) .->. (r2,s2,t2,u2) = ( r1 .->. r2 , s1 .->. s2 , t1 .->. t2 , u1 .->. u2 )
  (r1,s1,t1,u1) .==. (r2,s2,t2,u2) = ( r1 .==. r2 , s1 .==. s2 , t1 .==. t2 , u1 .==. u2 )

instance
  ( Propositional r
  , Propositional s
  , Propositional t
  , Propositional u
  , Propositional v
  ) => Propositional (r,s,t,u,v) where
  neg (r,s,t,u,v)                        = ( neg r      , neg s      , neg t      , neg u      , neg v      )
  (r1,s1,t1,u1,v1) .|.  (r2,s2,t2,u2,v2) = ( r1 .|.  r2 , s1 .|.  s2 , t1 .|.  t2 , u1 .|.  u2 , v1 .|.  v2 )
  (r1,s1,t1,u1,v1) .^.  (r2,s2,t2,u2,v2) = ( r1 .^.  r2 , s1 .^.  s2 , t1 .^.  t2 , u1 .^.  u2 , v1 .^.  v2 )
  (r1,s1,t1,u1,v1) .&.  (r2,s2,t2,u2,v2) = ( r1 .&.  r2 , s1 .&.  s2 , t1 .&.  t2 , u1 .&.  u2 , v1 .&.  v2 )
  (r1,s1,t1,u1,v1) .->. (r2,s2,t2,u2,v2) = ( r1 .->. r2 , s1 .->. s2 , t1 .->. t2 , u1 .->. u2 , v1 .->. v2 )
  (r1,s1,t1,u1,v1) .==. (r2,s2,t2,u2,v2) = ( r1 .==. r2 , s1 .==. s2 , t1 .==. t2 , u1 .==. u2 , v1 .==. v2 )

-- }}}

