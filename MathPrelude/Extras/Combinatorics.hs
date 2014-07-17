{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Extras.Combinatorics
  ( factorial
  , pochhammer
  , binomial
  , binomialED, binomialFd
  , sign
  ) where

import BasicPrelude
import MathPrelude.Algebraic.Field
import MathPrelude.Algebraic.EuclideanDomain
-- import MathPrelude.Constructions.Complex
-- import MathPrelude.Algebraic.Module
import MathPrelude.Common.Integral

factorial :: (Integral a, Ring b) => a -> b
factorial n = foldr (*) 1 . map fromIntegral $ [1..n]

pochhammer :: Ring a => a -> Integer -> a
pochhammer _ 0 = 1
pochhammer a n = a * pochhammer (a-1) (n-1)

binomial :: Integer -> Integer -> Integer
binomial n k = pochhammer n k `div` factorial k

binomialFd :: Field a => a -> Integer -> a
binomialFd n k = pochhammer n k / fromInteger (factorial k)

binomialED :: EuclideanDomain a => a -> Integer -> a
binomialED n k = pochhammer n k `div` fromInteger (factorial k)

sign :: (Integral a, Ring b) => a -> b
sign k
  | even k = 1
  | otherwise = -1
