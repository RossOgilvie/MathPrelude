{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
module MathPrelude.Extras.PolynomialFactorisation where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Polynomial
import MathPrelude.Structures.Complex
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

polyDiff :: (Ring a, NumEq a) => Poly a -> Poly a
polyDiff p = termwiseP polyDiff' p
polyDiff' n x = (n-1, n' * x)
		where n' = fromInteger $ convInteger n


-- (x,y) s.t. x*p + y*q = rhs
solveBezout p q rhs
	| m =~ zero = (x'', y'')
	| otherwise = (zero, zero)
		where
			g = gcd p q
			(f,m) = divMod rhs g
			(x,y) = extEuclidAlg p q
			x' = (x*f) `mod` q
			y' = (y*f) `mod` p
			u = stdUnit (x*p + y*q)
			x'' = x' `div` u
			y'' = y' `div` u
