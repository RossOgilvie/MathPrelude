{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
module MathPrelude.Extras.PolynomialFactorisation where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Polynomial
import MathPrelude.Structures.Complex
import MathPrelude.Structures.Derivation
import MathPrelude.Common.Integral

polyRootBound :: Poly (Complex Double) -> Double
polyRootBound p = min opt1 opt2
	where
		(x:xs) = map norm . reverse $ toList p
		opt1 = 1 + (maximum xs)/x
		opt2 = max 1 ((sum xs)/x)

findRoot :: Poly (Complex Double) -> Complex Double -> Complex Double
findRoot p x0 = newtons (evalP p) (evalP p') x0
	where p' = polyDiff p

converge :: (a -> a -> Bool) -> [a] -> a
converge _ [x] = x
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys


newton_step f f' x = x - (f x / f' x)

newtons f f' x0 = converge (=~) $ iterate (newton_step f f') x0
--newtons f f' x0 = take 10 $ iterate (newton_step f f') x0

linearPolyWithRoot c = poly [negate c, one]


-- Durand–Kerner method
factorPoly p = factorPoly' (stdAssociate p) initPts
	where
		deg = degreeP p
		omega = primitiveRoot deg
		offset = fromPolar (polyRootBound p) 1 -- roots of unity have pi*k/n args, and arg pi*k/n + 1 is never real
		initPts = map (offset *) . map (omega^) $ [1..deg]

factorPoly' p pts = converge (=~) . iterate (dkStep p []) $ pts

type PCD = Poly (Complex Double)
type CD = Complex Double
-- step through the list, taking from the front after, recomputing and putting onto the front of before
-- then in the final step, reverse the list to preserve the ordering
dkStep :: PCD -> [CD] -> [CD] -> [CD]
dkStep p before [] = reverse before
dkStep p before (old:after) = dkStep p (new : before) after
	where
		new = old - (evalP p old)/denom
		denom = product $ map (old -) (before++after)

-- 	where
-- 		uniqueFacs = snd $ head $ drop n $ iterate factorPoly' (psf,[])
-- 		psf = squareFree p
-- 		rr = repeatedRoots p
-- 		n = degreeP psf
--
-- factorPoly' p
-- 	| degree rr == 0 = factorPoly'' p p
-- 	where
-- 		rr = gcd p (derive p)
--
-- -- Factor a squarefree poly q.
-- -- the original poly is p, q is squarefree
-- factorPoly'' p q

factorRoot p x
	| evalP p x /=~ 0 = (p,0)
	| otherwise = (p', n+1)
			where (p', n) = factorRoot (p `div` linearPolyWithRoot x) x


instance Ring a => Derivation (Poly a) where
	derive = polyDiff

instance Field a => Integration (Poly a) where
	integrate = polyInt

polyDiff :: (Ring a, NumEq a) => Poly a -> Poly a
polyDiff p = termwiseP polyDiff' p
polyDiff' n x = (n-1, n' * x)
		where n' = fromIntegral n

polyInt :: (Field a, NumEq a) => Poly a -> Poly a
polyInt p = termwiseP polyInt' p
polyInt' n x = (n-1, x / n')
	where n' = fromIntegral n


-- (x,y) s.t. x*p + y*q = rhs
solveBezout p q rhs
	| m =~ zero = (x'', y'') -- only a sol if gcd | rhs
	| otherwise = (zero, zero)
		where
			g = gcd p q
			(f,m) = divMod rhs g -- factor out the gcd from rhs
			(x,y) = extEuclidAlg p q -- solve the related x*p + y*q = 1
			(d,x') = divMod (x*f) q -- (xf)p + (yf)q = f, but then reduce xf by the 'best possible' multiple of q
			y' = (y*f) + d*p -- reduce yf by the same multiple. The top degrees will be cancelled by the same factor
			u = stdUnit (x*p + y*q) -- my gcd gives the stdUnit, so have to put this factor back in
			x'' = x' `div` u
			y'' = y' `div` u
