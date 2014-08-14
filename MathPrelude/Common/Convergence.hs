{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
module MathPrelude.Common.Convergence
  (
  -- * Sequences
  eitherConverge
  , converge
  , converge'
  , converges
  -- * Series
  , partialSums
  , series
  , series'
  ) where

import BasicPrelude
import MathPrelude.Algebraic.Ring
import MathPrelude.Common.NumEq
import qualified Data.Either as E


-- | The supplied predicate is applied to successive terms of the sequence, and if it matches then that is returned in a Left. Otherwise the last term is returned in a Right
eitherConverge ∷ (a → a → Bool) → [a] → Either a a
eitherConverge _ [x] = Right x
eitherConverge p (x:ys@(y:_))
  | p x y     = Left y
  | otherwise = eitherConverge p ys

-- | Similiar to 'eitherConverge', returning True if it has returned after the specified number of term, and False if it has reached the end of those terms without converging.
converges ∷ (a → a → Bool) → Int → [a] → Bool
converges f n ls = E.isLeft $ eitherConverge f (take n ls)

-- | Returns the limit or the last term, using the predicate '=~'.
converge ∷ NumEq a ⇒ [a] → a
converge = regardless . eitherConverge (=~)

-- | Returns the limit or the last term, using the supplied predicate.
converge' ∷ (a → a → Bool) → [a] → a
converge' p = regardless . eitherConverge p

regardless ∷ Either a a → a
regardless (Left x) = x
regardless (Right x) = x

-- | Computes the partial sums of the given list using the monoid operation, starting with the first term given.
partialSums ∷ Monoid a ⇒ [a] → [a]
partialSums ls = tail ps
  where ps = zero : zipWith (<>) ps ls

-- | Compute the sum of a series, given a formula for the terms, starting from 0.
series ∷ (NumEq a, Monoid a) ⇒ (Int → a) → a
series f = converge . partialSums . map f $ [0..]

-- | Compute the sum of a series, given a formula for the terms, starting from the given integer.
series' ∷ (NumEq a, Monoid a) ⇒ (Int → a) → Int → a
series' f k = converge . partialSums . map f $ [k..]
