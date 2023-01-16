{-# OPTIONS -Wall #-}

module Util
  ( takeWhileInclusive,
  )
where

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive p = foldr (\x ys -> if p x then x : ys else [x]) []