{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# LANGUAGE BangPatterns         #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module MathPrelude.Calculus.Integration
  (
  module MathPrelude.Classes.Module
  , numIntegrate
  , trapezoid
  , simpsons
  ) where

import MathPrelude
import           MathPrelude.Classes.Module
import           MathPrelude.Calculus.Convergence


-- | Numerical integration of a function on from the lower bound to the uper bound via Simpons' method with an increasingly fine partition.
-- This is done internally by simpsons_nested in an efficient manner, so just supply a large upper bound on the number of steps
numIntegrate ∷ (Field a, Approx b, Field b, Module b a) ⇒ (a→b)→a→a→b
numIntegrate f a b = simpsons f a b 1000

-- | The trapezoid rule for a function with the specified number of partitions.
trapezoid ∷ (Field a, Field b, Module b a) ⇒ (a→b) → a → a → Integer → b
trapezoid f a b n = h .* sum (map f xs)
  where
    h = (b-a) / fromInteger n
    xs = map (\k → a + (k+1)*h) . map fromInteger $ [0..(n-1)]

-- | Simpons' rule for a function with the specified number of partitions.
simpsons ∷ (Field a, Field b, Approx b, Module b a) ⇒ (a→b) → a → a → Int → b
simpsons = simpsons_nested

simpsons_classic ∷ (Field a, Field b, Module b a) ⇒ (a→b) → a → a → Int → b
simpsons_classic f a b n = (h / 3) .* (fa + fb + 2*sum evens + 4*sum odds)
  where
    fa = f a
    fb = f b
    h = (b-a) / fromIntegral (2*n)
    evens = map f . map (\k → a + 2*k*h) . map fromIntegral  $ [1..(n-1)]
    odds = map f . map (\k → a + (2*k + 1)*h) . map fromIntegral $ [0..(n-1)]

simpsons_nested ∷ (Field a, Field b, Approx b, Module b a) ⇒ (a→b) → a → a → Int → b
simpsons_nested f a b maxSteps = converge simps
    where
        fa = f a
        fb = f b
        width n = (b-a)/ fromIntegral n

        ns = map (2^) [1..maxSteps] ∷ [Int]
        odd_integers n = [1,3..(n-1)]
        odd_points n = map (+a) . map (*width n) . map fromIntegral . odd_integers $ n

        odd_row_sum = sum . map f . odd_points
        row_sums = map odd_row_sum ns
        partial_sums = 0 : partialSums row_sums -- the initial level has no evens, is one step behind
        widths = map width ns

        simps = zipWith3 (\w e o → (w/3) .* (2*e + 4*o + fa + fb)) widths partial_sums row_sums
