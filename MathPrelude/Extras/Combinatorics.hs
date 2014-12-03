{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
module MathPrelude.Extras.Combinatorics
  (
  -- * Factorials
  factorial
  , dble_factorial
  , pochhammer
  -- * Binomial Coefficients
  -- $bino
  , binomial, binomialRow
  , binomialED, binomialFd
  -- * Catalan Numbers
  -- $catalan
  , catalan, catalans
  -- * Stirling Numbers
  -- $stirling
  , stirling, stirlingRow
  -- * Misc
  , sign
  ) where

import BasicPrelude
import MathPrelude.Algebraic.Field
import MathPrelude.Algebraic.EuclideanDomain
-- import MathPrelude.Constructions.Complex
-- import MathPrelude.Algebraic.Module
import MathPrelude.Common.Integral

-- | The factorial function. Returns 1 on negative numbers.
-- <https://en.wikipedia.org/wiki/Factorial Wikipedia>
factorial ∷ (Integral a, Ring b) ⇒ a → b
factorial n = product . map fromIntegral $ [1..n]

-- | The double factorial function. Not defined on negative odd numbers, as we are resticted by the ring instance. Instead returns 1 on negative numbers.
-- <https://en.wikipedia.org/wiki/Double_factorial Wikipedia>
dble_factorial ∷ (Integral a, Ring b) ⇒ a → b
dble_factorial n
  | even n = product . map fromIntegral $ [2,4..n]
  | otherwise = product . map fromIntegral $ [1,3..n]

-- | The Pochhammer function. Also known as the falling factorial. Returns 0 when the second argument is negative.
-- <https://en.wikipedia.org/wiki/Pochhammer_symbol Wikipedia>
pochhammer ∷ Ring a ⇒ a → Integer → a
pochhammer a n
  | n < 0 = 0
  | otherwise = pochhammer' a n
pochhammer' _ 0 = 1
pochhammer' a n = a * pochhammer' (a-1) (n-1)

-- | Compute a single binomial coefficent.
binomial ∷ Integer → Integer → Integer
binomial n k = pochhammer n k `div` factorial k

-- $bino
-- <https://en.wikipedia.org/wiki/Binomial_coefficient Wikipedia>

-- | Compute a whole row of binomial coefficents. More efficent than computing them individually.
binomialRow ∷ Integer → [Integer]
binomialRow n = take (fromInteger n+1) result
  where
    result = 1 : zipWith3 (\b nu de → (b* nu) `div` de) result nums denoms
    nums = map (\k → n-k+1) [1..]
    denoms = [1..]

-- | Compute a single binomial coefficent from the formula, using the field division.
binomialFd ∷ Field a ⇒ a → Integer → a
binomialFd n k = pochhammer n k / fromInteger (factorial k)

-- | Compute a single binomial coefficent from the formula, using the integral division.
binomialED ∷ EuclideanDomain a ⇒ a → Integer → a
binomialED n k = pochhammer n k `div` fromInteger (factorial k)

-- $catalan
-- <https://en.wikipedia.org/wiki/Catalan_number Wikipedia>

-- | 1 for even numbers, -1 for odd.
sign ∷ (Integral a, Ring b) ⇒ a → b
sign k
  | even k = 1
  | otherwise = -1

-- | The Catalan numbers in a list starting with [1,1,2,..].
catalans ∷ [Integer]
catalans = 1 : map catalans' [1..]
catalans' k = sum $ zipWith (*) cat (reverse cat)
   where
     cat = take k catalans

-- | Compute a specific Catalan from the formula.
catalan n = binomial (2*n) n `div` (n+1)

-- $stirling
-- <https://en.wikipedia.org/wiki/Stirling_numbers_of_the_second_kind>
-- | Compute a row of Stirling numbers of the second kind, ie S(n,k) for k=0,..,n
stirlingRow ∷ Integer → [Integer]
stirlingRow 0 = [1]
stirlingRow n = 0 : zipWith3 (\k o t → k*o + t) [1..] (drop 1 s ++ [0]) s
  where s = stirlingRow (n-1)

-- | Compute the Stirling numbers of the second kind, ie S(n,k)
stirling ∷ Integer → Integer → Integer
stirling n k
  | n < 0 = 0
  | k < 0 || k > n = 0
  | k == n = 1
  | n > 0 && k == 0 = 0
  | n > 0 && k == 1 = 1
  | otherwise = (stirlingRow n) !! fromIntegral k
