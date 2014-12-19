{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
module MathPrelude.Tests.PolyTest() where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Constructions.Polynomial
import MathPrelude.Extras.PolynomialFactorisation

import MathPrelude.Classes.Transcendental
import MathPrelude.Classes.Real
import MathPrelude.Classes.Rational
import MathPrelude.Constructions.Complex
import MathPrelude.Classes.Derivation

import Test.QuickCheck


-----------------------------------
--- QuickCheck
-----------------------------------

instance (Monoid a, Arbitrary a) => Arbitrary (Poly a) where
	arbitrary = do
		ls <- arbitrary -- :: Gen [a]
		return $ fromListP ls

-- instance Arbitrary a => Arbitrary (Complex a) where
-- 	arbitrary = do
-- 		x <- arbitrary
-- 		y <- arbitrary
-- 		return $ x :+ y

prop_construct xs = not (null xs) ==> (toListP . fromListP) xs == xs
prop_destruct p = (fromListP . toListP) p == p

type PD = Poly Double
prop_add_assc :: PD -> PD -> PD -> Bool
prop_add_assc p q r = (p+q)+r =~ p + (q+r)
prop_add_identity :: PD -> Bool
prop_add_identity p = p+zero =~ p
prop_add_inverse :: PD -> Bool
prop_add_inverse p = p - p =~ zero
prop_add_comm :: PD -> PD -> Bool
prop_add_comm p q = p + q =~ q + p

prop_mul_assc :: PD -> PD -> PD -> Bool
prop_mul_assc p q r = (p*q)*r =~ p*(q*r)
prop_mul_identity :: PD -> Bool
prop_mul_identity p = p*one =~ p
prop_mul_comm :: PD -> PD -> Bool
prop_mul_comm p q = p * q =~ q * p

prop_dist :: PD -> PD -> PD -> Property
prop_dist p q r = collect (degreeP p) $ p * (q + r) =~ p*q + p*r

prop_gen_dist p qs = collect (length qs) $ p * sum qs =~ sum (map (p*) qs)
	where types = p :: PD

-- prop_div :: Poly Double -> Poly Double -> Property
prop_div p q =
	q /=~ zero ==>
		collect (degreeP p, degreeP q) $
		p =~ (d*q + m)
	where
		(d,m) = divMod p q
		types = p :: PD

prop_gcd p q =
	p /=~ zero ==>
	q /=~ zero ==>
		collect (degreeP g) $ nearZero (mod p g)
	where
		g = gcd p q
		types = p :: PD









------------------------------------------
---- Statistically testing division
------------------------------------------



-- polyNorm p = sqrt . sum . map (^2) . map snd $ ls
polyNormAbs :: Poly Double -> Double
polyNormAbs = maximum . map P.abs . toListP
polyNormRel :: (Poly Double, Poly Double) -> Double
polyNormRel (p, q) = (maximum . map P.abs . toListP $ (p-q)) / r
	where r = maximum . map P.abs $ toListP p ++ toListP q

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

div_make_data_pairs = do
	polys <- div_make_data div_stats_size
	return $ div_make_pairs polys

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

div_stats_pass_fail = do
	polys <- div_make_data div_stats_size
	let pairs = div_make_pairs polys
	let test = map (uncurry (=~)) pairs
	let (p,f) = partition id $ test
	print $ "pass:" ++ show (length p) ++ ", fail: " ++ show (length f)
	-- sequence_ . map putStrLn . map (\(d,n) -> show d ++ ": " ++ show n) $ datums

-- div_stats_abs_bucket :: IO [(Double,Int)]
div_stats_abs_bucket = do
	(_,_,_,good) <- div_make_abs_stats
	let datums = grouping . map fromInteger . sort . map floor $ good :: [(Double,Int)]
	sequence_ . map putStrLn . map (\(d,n) -> show d ++ ": " ++ show n) $ datums

-- div_stats_rel_bucket :: IO [(Double,Int)]
div_stats_rel_bucket = do
	(_,_,_,good) <- div_make_rel_stats
	let datums = grouping . map fromInteger . sort . map floor $ good :: [(Double,Int)]
	sequence_ . map putStrLn . map (\(d,n) -> show d ++ ": " ++ show n) $ datums


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


prop_find_roots p = map (act p) (findRoots p) =~ take (degreeP p) (repeat 0)

prop_factor_root x p n =
	n > 0 ==>
	act p x /=~ 0 ==>
	snd (removeRoot (y^n *p) x) == n
	where y = fromListP [-x,1]


c = 31.793125588403605 :+ 0.0 :: Complex Double
y = fromListP [-c,1] :: Poly (Complex Double)
powers_test = takeWhile (=~0) . map (\p -> act p c) . map (y^) $ [1..]
inf_division_test p = takeWhile (=~p) . map (division_test p) $ [1..]
division_test q n = (!!(n-1)) . iterate (`div` q) $ (q^n)
prop_division_test p =
	p /=~ 0 ==>
		collect (degreeP p) $
		division_test p 10 =~ p
