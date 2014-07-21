{-# LANGUAGE RebindableSyntax, BangPatterns #-}
module MathPrelude.Extras.SpecialFunc
  ( module MathPrelude.Constructions.Complex
  , gammaF
  , zetaF
  , euler_const
  , li
  , erf
  ) where


-----------------------------------------
-- Imports
-----------------------------------------
import BasicPrelude
import MathPrelude.Algebraic.Field
import MathPrelude.Algebraic.EuclideanDomain
import MathPrelude.Constructions.Complex
import MathPrelude.Common.Convergence
import MathPrelude.Common.Integral
import MathPrelude.Common.Rational
import MathPrelude.Common.Transcendental
import MathPrelude.Extras.Primes
import MathPrelude.Extras.Combinatorics

-----------------------------------------
-- Gamma Function
-----------------------------------------
gammaF :: Complex Double -> Complex Double
gammaF z'
  | realPart z' >= 0.5 = part1 * part2 * part3 * part4
  | otherwise = pi / sin (pi*z') / gammaF (1-z')
  where
    z = z'-1
    -- Coefficients used by the GNU Scientific Library
    g = 7
    ck = [0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313, -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7]
    part1 = sqrt (2*pi)
    part2 = (z + g + 0.5)**(z + 0.5)
    part3 = exp (negate (z + g + 0.5))
    part4 = (ck!!0) + sum (zipWith (\c k -> c/(z+fromReal k)) (tail ck) [1..])


gammaF' :: Double -> Double
gammaF' = realPart . gammaF . fromReal

-----------------------------------------
-- Zeta Function
-----------------------------------------
zetaF :: Complex Double -> Complex Double
zetaF = zetaF_Euler

zetaF_Hasse s = factor * converge (partialSums outer_sum)
  where
    factor = 1/(1-2**(1-s))
    outer_sum = map (\n -> inner_sum n / 2^(fromInteger n+1)) [0..]
    inner_sum n = sum . map (\k -> sign k * fromInteger (binomial n k) * (fromInteger k+1)**(-s)) $ [0..n]

zetaF_Euler s
  | realPart s >= 1 = converge $ partialprods
  | otherwise = reflection
  where
    euler p = 1/(1-p**(-s))
    terms = map euler . map fromInteger $ primes
    partialprods = 1 : zipWith (*) partialprods terms
    reflection = (2*pi)**s / pi * sin (pi*s/2) * gammaF (1-s) * zetaF (1-s)

-----------------------------------------
-- Euler's Constant
-----------------------------------------

euler_const :: Double
euler_const = 0.57721566490153286


-----------------------------------------
-- Logarithmic Integral
-----------------------------------------

li x = euler_const + log (log x) + sqrt x * res
  where
    res = converge $ partialSums outer
    outer = map outer_term [1..]
    outer_term n = sign (n-1) * (log x)^n / factorial n / fromInteger (2^(n-1)) * inner n
    inner n = sum $ map (\k -> 1/fromIntegral (2*k +1)) [0..((n-1)`div`2)]

-----------------------------------------
-- Error function
-----------------------------------------

erf :: Double -> Double
erf x
  | x > 4.83 = 1
  | x < (-4.83) = -1
  | otherwise = erf_prod x

erf_series z = 2 / sqrt pi * converge (partialSums terms)
  where
    terms = map term [0..]
    term n = sign n * z^(2*n+1)/factorial n / fromIntegral (2*n+1)

erf_prod z = 2 / sqrt pi * converge (partialSums terms)
  where
    terms = z : zipWith (*) terms mult
    mult = map (\k -> -(2*k -1) * z*z/k/(2*k+1)) [1..]
