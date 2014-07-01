{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
module MathPrelude.Tests.PolyTest() where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Polynomial
import MathPrelude.Extras.PolynomialFactorisation

import MathPrelude.Common.Transcendental
import MathPrelude.Common.Real
import MathPrelude.Common.CharZero
import MathPrelude.Structures.Complex
import MathPrelude.Structures.Derivation

import Test.QuickCheck


-----------------------------------
--- QuickCheck
-----------------------------------

instance (Monoid a, Arbitrary a) => Arbitrary (Poly a) where
	arbitrary = do
		ls <- arbitrary -- :: Gen [a]
		return $ poly ls

instance Arbitrary a => Arbitrary (Complex a) where
	arbitrary = do
		x <- arbitrary
		y <- arbitrary
		return $ x :+ y

prop_construct xs = not (null xs) ==> (toList . poly) xs == xs
prop_destruct p = (poly . toList) p == p

prop_add_assc p q r = (p+q)+r =~ p + (q+r)
prop_add_identity p = p+zero =~ p
prop_add_inverse p = p - p =~ zero
prop_add_comm p q = p + q =~ q + p

prop_mul_assc p q r = (p*q)*r =~ p*(q*r)
prop_mul_identity p = p*one =~ p
prop_mul_inverse p =
	p /=~ zero ==> p * recip p =~ one
prop_mul_comm p q = p * q =~ q * p

prop_dist p q r = p * (q + r) =~ p*q + p*r

prop_div :: Poly Double -> Poly Double -> Property
prop_div p q =
	q /=~ zero ==> p =~ (d*q + m)
		where (d,m) = divMod p q

-- polyNorm p = sqrt . sum . map (^2) . map snd $ ls
polyNormAbs :: Poly Double -> Double
polyNormAbs = maximum . map P.abs . toList
polyNormRel :: (Poly Double, Poly Double) -> Double
polyNormRel (p, q) = (maximum . map P.abs . toList $ (p-q)) / r
	where r = maximum . map P.abs $ toList p ++ toList q

div_make_data :: Int -> IO [Poly Double]
div_make_data n = do
		gs <- sequence . take n . repeat . generate $ (arbitrary :: Gen (Poly Double))
		return gs

div_make_pairs :: [Poly Double] -> [(Poly Double, Poly Double)]
div_make_pairs [] = []
div_make_pairs [_] = []
div_make_pairs (x:y:xs) = (x, d*y + m) : div_make_pairs xs
	where (d,m) = divMod x y

div_stats_size = 2000

-- div_make_stats :: IO ()
div_make_abs_stats = do
	polys <- div_make_data div_stats_size
	let pairs = div_make_pairs polys
	let errors = map (\(p,q) -> polyNormAbs $ p-q) pairs
	let mags = map (logBase 10) errors
	let (infs,mags') = partition P.isInfinite mags
	let (nans,good) = partition P.isNaN mags'
	return (mags,infs,nans,good)

div_make_rel_stats = do
	polys <- div_make_data div_stats_size
	let pairs = div_make_pairs polys
	let errors = map polyNormRel pairs
	let mags = map (logBase 10) errors
	let (infs,mags') = partition P.isInfinite mags
	let (nans,good) = partition P.isNaN mags'
	return (mags,infs,nans,good)

div_stats_raw :: IO Text
div_stats_raw = do
	(_,infs,nas,good) <- div_make_abs_stats
	return $ "Exact: " ++ show (length infs) ++ ", NaNs: " ++ show (length nas) ++ ", Mean: " ++ show (mean good) ++ ", StdDev: " ++ show (stdDev good)
--
-- -- <= epsilon P.* P.maximum [oneDouble, P.abs x, P.abs y]
--
div_stats_abs_bucket :: IO [(Double,Int)]
div_stats_abs_bucket = do
	(_,_,_,good) <- div_make_abs_stats
	return $ grouping . map fromInteger . sort . map floor $ good

div_stats_rel_bucket :: IO [(Double,Int)]
div_stats_rel_bucket = do
	(_,_,_,good) <- div_make_rel_stats
	return $ grouping . map fromInteger . sort . map floor $ good


grouping l = grouping' (zip l (repeat 1))
grouping' [] = []
grouping' [s] = [s]
grouping' ((x,n):(y,_):xs)
	| x == y = grouping' ((x, n+1) : xs)
	| otherwise = (x,n) : grouping' ((y,1) : xs)


mean l = (sum l) / P.fromIntegral (length l)
stdDev l = sqrt . (/n) . sum . map d $ l
	where
		n = P.fromIntegral . length $ l
		m = mean l
		d x = (x-m)^2


prop_find_roots p = map (evalP p) (factorPoly p) =~ take (degreeP p) (repeat 0)

prop_factor_root x p n =
	n > 0 ==>
	evalP p x /=~ 0 ==>
	snd (factorRoot (y^n *p) x) == n
	where y = linearPolyWithRoot x
