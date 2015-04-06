
module Data.Logic.Modal where

data Modal r
  = Modal (Modal r)
  | Unmodal r
  deriving (Eq,Ord,Show)



