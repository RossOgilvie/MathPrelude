{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
module MathPrelude.Common.Convergence
  ( eitherConverge
  , converge
  , converge'
  , converges
  , partialSums
  , series
  , series'
  ) where

import BasicPrelude
import MathPrelude.Algebraic.Ring
import MathPrelude.Classes.NumEq
import qualified Data.Either as E


-- | The supplied predicate is applied to successive terms of the sequence, and if it matches then that is returned in a Left. Otherwise the last term is returned in a Right
eitherConverge :: (a -> a -> Bool) -> [a] -> Either a a
eitherConverge _ [x] = Right x
eitherConverge p (x:ys@(y:_))
  | p x y     = Left y
  | otherwise = eitherConverge p ys

converges :: (a -> a -> Bool) -> Int -> [a] -> Bool
converges f n ls = E.isLeft $ eitherConverge f (take n ls)

converge :: NumEq a => [a] -> a
converge = regardless . eitherConverge (=~)

converge' :: (a → a → Bool) → [a] → a
converge' p = regardless . eitherConverge p

regardless :: Either a a -> a
regardless (Left x) = x
regardless (Right x) = x

partialSums :: Monoid a => [a] -> [a]
partialSums ls = tail ps
  where ps = zero : zipWith (<>) ps ls

series :: (NumEq a, Monoid a) ⇒ (Int → a) -> a
series f = converge . partialSums . map f $ [0..]

series' :: (NumEq a, Monoid a) ⇒ (Int → a) → Int → a
series' f k = converge . partialSums . map f $ [k..]
