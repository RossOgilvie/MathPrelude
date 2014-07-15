{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Extras.Sequence
  ( maybeConverge
  , converge
  , converges
  ) where

import BasicPrelude
import MathPrelude.Structures.NumEq
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
