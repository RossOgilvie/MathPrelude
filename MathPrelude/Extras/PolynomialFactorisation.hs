{-# LANGUAGE RebindableSyntax, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
module MathPrelude.Extras.PolynomialFactorisation
	( findRoot
	, findRoots
	, removeRoot
	, rootOrder
	, isRoot
	, solveBezout
	) where

import BasicPrelude

import MathPrelude.Constructions.Polynomial
import MathPrelude.Constructions.Complex
import MathPrelude.Classes.Derivation
import MathPrelude.Common.Integral
import MathPrelude.Common.Transcendental
import MathPrelude.Common.Convergence
import MathPrelude.Extras.NewtonsMethod

-- all the roots of a poly lie in this circle
polyRootBound :: Poly (Complex Double) -> Double
polyRootBound p = min opt1 opt2
	where
		(x:xs) = map norm . reverse $ toListP p
		opt1 = 1 + (maximum xs)/x
		opt2 = max 1 ((sum xs)/x)

-- use newtons method to find a root
findRoot :: Poly (Complex Double) -> Complex Double -> Complex Double
findRoot p x0 = newtons (p$$) (p'$$) x0
	where p' = derive p

w :: Poly (Complex Double)
w = product . map linearPolyWithRoot . map fromReal $ [1..10]

findRealRoots :: Poly Double -> [Complex Double]
findRealRoots p = findRoots $ map fromReal p

findRoots :: Poly (Complex Double) -> [Complex Double]
findRoots p = concatMap (duplicate . refine) crudeRoots
	where
		repeated = gcd' p (derive p)
		squarefree = if degreeP repeated > 0 then p `div` repeated else p
		crudeRoots = dkMethod squarefree
		refine x
			| isRoot p x = x
			| otherwise = findRoot p x
		duplicate x = replicate (rootOrder p x) x

removeRoot :: Field a => Poly a -> a -> (Poly a, Int)
removeRoot p x
	| isRoot p x = (p,0)
	| otherwise = (p', n+1)
			where (p', n) = removeRoot (p `div` linearPolyWithRoot x) x

-- rootOrder :: Field a => Poly a -> a -> Int
rootOrder p x = length . takeWhile (flip isRoot x) . take (degreeP p) . iterate derive $ p
-- rootOrder p x = takeWhile (flip isRoot x) . take (degreeP p) . iterate derive $ p
-- rootOrder p x = iterate derive $ p

isRoot :: Ring a => Poly a -> a -> Bool
isRoot p x = smallL (toListP p) $ eval p x
linearPolyWithRoot c = poly [negate c, one]

--------------------------
-- Durand–Kerner method
--------------------------
dkMethod :: Poly (Complex Double) -> [Complex Double]
dkMethod p = converge . iterate (dkStep (stdAssociate p) []) $ dkInitPts p

-- The initial points for the DK method should be spread throughout the potential disk, and not real
dkInitPts :: Poly (Complex Double) -> [Complex Double]
dkInitPts p = initPts
	where
		deg = degreeP p
		omega = 2*pi/fromIntegral deg
		args = map (\k -> 1 + omega*k) . map fromIntegral $ [1..deg] -- roots of unity have pi*k/n args, and arg pi*k/n + 1 is never real
		r = polyRootBound p
		radii = map (\k -> k*r/fromIntegral deg) . map fromIntegral $ [1..deg]
		initPts = zipWith fromPolar radii args

-- step through the list, taking from the front of after, recomputing and putting onto the front of before
-- then in the final step, reverse the list to preserve the ordering
dkStep :: Poly (Complex Double) -> [Complex Double] -> [Complex Double] -> [Complex Double]
dkStep p before [] = reverse before
dkStep p before (old:after) = dkStep p (new : before) after
	where
		new = old - (p $$ old)/denom
		denom = product $ map (old -) (before++after)

----------------------------------
-- Bezout's method
----------------------------------
-- (x,y) s.t. x*p + y*q = rhs
solveBezout p q rhs
	| m <<~ f = (x', y') -- only a sol if gcd | rhs
	| otherwise = (zero, zero)
		where
			g = gcd' p q -- use the un-normalised gcd
			(f,m) = divMod rhs g -- factor out the gcd from rhs
			(x,y) = extEuclidAlg p q -- solve the related x*p + y*q = g
			(d,x') = divMod (x*f) q -- (xf)p + (yf)q = fg = rhs, but then reduce xf by the 'best possible' multiple of q
			y' = (y*f) + d*p -- reduce yf by the same multiple. The higher degrees will be cancelled by the same factor
