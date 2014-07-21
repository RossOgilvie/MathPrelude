{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Extras.Combinatorics
  ( factorial
  , dble_factorial
  , pochhammer
  , binomial, binomialRow
  , binomialED, binomialFd
  , sign
  , catalan, catalans
  ) where

import BasicPrelude
import MathPrelude.Algebraic.Field
import MathPrelude.Algebraic.EuclideanDomain
-- import MathPrelude.Constructions.Complex
-- import MathPrelude.Algebraic.Module
import MathPrelude.Common.Integral

-- | The factorial function. Returns 1 on negative numbers.
-- <https://en.wikipedia.org/wiki/Factorial Wikipedia>
factorial :: (Integral a, Ring b) => a -> b
factorial n = foldr (*) 1 . map fromIntegral $ [1..n]

-- | The double factorial function. Not defined on negative odd numbers, as we are resticted by the ring instance. Instead returns 1 on negative numbers.
-- <https://en.wikipedia.org/wiki/Double_factorial Wikipedia>
dble_factorial :: (Integral a, Ring b) => a -> b
dble_factorial n
  | even n = foldr (*) 1 . map fromIntegral $ [2,4..n]
  | otherwise = foldr (*) 1 . map fromIntegral $ [1,3..n]

-- | The Pochhammer function. Also known as the falling factorial. Returns 0 when the second argument is negative.
-- <https://en.wikipedia.org/wiki/Pochhammer_symbol Wikipedia>
pochhammer :: Ring a => a -> Integer -> a
pochhammer a n
  | n < 0 = 0
  | otherwise = pochhammer' a n
pochhammer' _ 0 = 1
pochhammer' a n = a * pochhammer' (a-1) (n-1)

-- | Compute a single binomial coefficent.
binomial :: Integer -> Integer -> Integer
binomial n k = pochhammer n k `div` factorial k

-- | Compute a whole row of binomial coefficent. More efficent than computing them individually.
binomialRow :: Integer -> [Integer]
binomialRow n = take (fromInteger n+1) result
  where
    result = 1 : zipWith3 (\b nu de -> (b* nu) `div` de) result nums denoms
    nums = map (\k -> n-k+1) [1..]
    denoms = [1..]

-- | Compute a single binomial coefficent from the formula, using the field division.
binomialFd :: Field a => a -> Integer -> a
binomialFd n k = pochhammer n k / fromInteger (factorial k)

-- | Compute a single binomial coefficent from the formula, using the integral division.
binomialED :: EuclideanDomain a => a -> Integer -> a
binomialED n k = pochhammer n k `div` fromInteger (factorial k)

-- | 1 for even numbers, -1 for odd.
sign :: (Integral a, Ring b) => a -> b
sign k
  | even k = 1
  | otherwise = -1

-- | The Catalan numbers in a list starting with [1,1,2,..].
-- <https://en.wikipedia.org/wiki/Catalan_number Wikipedia>
catalans :: [Integer]
catalans = 1 : map catalans' [1..]
catalans' k = sum $ zipWith (*) cat (reverse cat)
   where
     cat = take k catalans

-- | Compute a specific Catalan from the formula.
catalan n = binomial (2*n) n `div` (n+1)
