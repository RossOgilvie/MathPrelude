{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
module MathPrelude.Extras.PolynomialFactorisation where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Polynomial
import MathPrelude.Structures.Complex
import MathPrelude.Structures.Derivation
import MathPrelude.Common.Integral

repeatedRoots :: (Field a, NumEq a) => Poly a -> Poly a
repeatedRoots p = gcd p p'
	where p' = polyDiff p

squareFree p = p `div` repeatedRoots p

initSearchPt = 10 :+ 10 :: Complex Double
findRoot p = newtons (polyEval p) (polyEval p') initSearchPt
	where p' = polyDiff p

converge :: (a -> a -> Bool) -> [a] -> a
converge _ [x] = x
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys


newton_step f f' x = x - (f x / f' x)

newtons f f' x0 = converge (=~) $ iterate (newton_step f f') x0
--newtons f f' x0 = take 10 $ iterate (newton_step f f') x0

linearPoly c = poly [negate c, one]

factorPoly p
	| n == 0 = []
	| otherwise = uniqueFacs ++ factorPoly rr
		where
			uniqueFacs = snd $ head $ drop n $ iterate factorPoly' (psf,[])
			psf = squareFree p
			rr = repeatedRoots p
			n = degreeP psf

factorPoly' (p,roots) = (p `div` fac, root : roots)
	where
		root = findRoot p
		fac = linearPoly root

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
