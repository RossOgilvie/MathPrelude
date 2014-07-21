{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Common.Convergence
  ( maybeConverge
  , converge
  , converges
  , partialSums
  ) where

import BasicPrelude
import MathPrelude.Algebraic.Abelian
import MathPrelude.Classes.NumEq
import qualified Data.Maybe as M

maybeConverge :: (a -> a -> Bool) -> [a] -> Maybe a
maybeConverge _ [x] = Nothing
maybeConverge p (x:ys@(y:_))
  | p x y     = Just y
  | otherwise = maybeConverge p ys

converges :: (a -> a -> Bool) -> Int -> [a] -> Bool
converges f n ls = M.isJust $ maybeConverge f (take n ls)

converge :: NumEq a => [a] -> a
converge xs
  | M.isJust x' = M.fromJust x'
  | otherwise = last xs
  where x' = maybeConverge (=~) xs

partialSums :: Monoid a => [a] -> [a]
partialSums ls = tail ps
  where ps = zero : zipWith (<>) ps ls
