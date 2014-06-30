{-# LANGUAGE RebindableSyntax, OverloadedStrings, RankNTypes #-}
module MathPrelude.Tests.DerivationTest where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Derivation
import MathPrelude.Structures.Ring
import MathPrelude.Structures.Polynomial
import MathPrelude.Structures.Quotient

-- import MathPrelude.Common.Floating
import MathPrelude.Extras.PolynomialFactorisation
import MathPrelude.Tests.PolyTest
import MathPrelude.Tests.QuotientTest

import Test.QuickCheck


-----------------------------------
--- QuickCheck
-----------------------------------

prop_const x = derive x =~ zero

prop_leibniz x y = derive (x*y) =~ derive x * y + x * derive y


allTypes1 :: (forall a. (Derivation a, Abelian a) => a -> Bool) -> IO ()
allTypes1 x = sequence_
    [ quickCheck (x :: Integer -> Bool)
    , quickCheck (x :: Int -> Bool)
    , quickCheck (x :: Int32 -> Bool)
    , quickCheck (x :: Int64 -> Bool)
    , quickCheck (x :: Float -> Bool)
    , quickCheck (x :: Double -> Bool)
    ]

allTypes1 :: (forall a. (Derivation a, Abelian a) => a -> Bool) -> IO ()
allTypes1 x = sequence_
    [ quickCheck (x :: Integer -> Bool)
    , quickCheck (x :: Int -> Bool)
    , quickCheck (x :: Int32 -> Bool)
    , quickCheck (x :: Int64 -> Bool)
    , quickCheck (x :: Float -> Bool)
    , quickCheck (x :: Double -> Bool)
    ]

prop_leibniz_poly :: Poly Double -> Poly Double -> Bool
prop_leibniz_poly = prop_leibniz

prop_leibniz_rat :: Quotient (Poly Double) -> Quotient (Poly Double) -> Bool
prop_leibniz_rat = prop_leibniz

-- instance (Monoid a, Arbitrary a) => Arbitrary (Poly a) where
--   arbitrary = do
--     ls <- arbitrary -- :: Gen [a]
--     return $ poly ls

-- polyNorm p = sqrt . sum . map (^2) . map snd $ ls
-- polyNorm :: Poly Double -> Double
-- polyNorm = maximum . map P.abs . toList
--
-- prop_construct xs = not (null xs) ==> (toList . poly) xs == xs
-- prop_destruct p = (poly . toList) p == p
--
-- prop_add_assc p q r = (p+q)+r =~ p + (q+r)
-- prop_add_identity p = p+zero =~ p
-- prop_add_inverse p = p - p =~ zero
-- prop_add_comm p q = p + q =~ q + p
--
-- prop_mul_assc p q r = (p*q)*r =~ p*(q*r)
-- prop_mul_identity p = p*one =~ p
-- prop_mul_inverse p =
--   p /=~ zero ==> p * recip p =~ one
-- prop_mul_comm p q = p * q =~ q * p
--
-- prop_dist p q r = p * (q + r) =~ p*q + p*r
--
-- prop_div :: Poly Double -> Poly Double -> Property
-- prop_div p q =
--   q /=~ zero ==> p =~ (d*q + m)
--     where (d,m) = divMod p q
--
-- div_error :: Poly Double -> Poly Double -> Double
-- div_error p q = polyNorm $ p - (d*q + m)
--   where (d,m) = divMod p q
--
-- div_stats = do
--     gs <- sequence . take 2000 . repeat . generate $ (arbitrary :: Gen (Poly Double))
--     let diffs = map (logBase 10) . f $ gs :: [Double]
--     let (infs,others) = partition P.isInfinite diffs
--     let (nas,nis) = partition P.isNaN others
--     return $ "Exact: " ++ show (length infs) ++ ", NaNs: " ++ show (length nas) ++ ", Mean: " ++ show (mean nis) ++ ", StdDev: " ++ show (stdDev nis)
-- -- 		return gs
--     where
--       f [] = []
--       f [_] = []
--       f (x:y:xs) = div_error x y : f xs
--
-- mean l = (sum l) / P.fromIntegral (length l)
-- stdDev l = sqrt . (/n) . sum . map d $ l
--   where
--     n = P.fromIntegral . length $ l
--     m = mean l
--     d x = (x-m)^2
