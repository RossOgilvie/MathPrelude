{-# LANGUAGE RebindableSyntax, MultiParamTypeClasses, FlexibleInstances, BangPatterns#-}
module MathPrelude
	( module BasicPrelude
	, module MathPrelude.Structures.Quotient
	, module MathPrelude.Structures.Module
	, module MathPrelude.Common.Integral
	, module MathPrelude.Common.CharZero
	, module MathPrelude.Common.Floating
	, module MathPrelude.Common.Real
	) where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Polynomial
import MathPrelude.Structures.Complex
import MathPrelude.Structures.Module
import MathPrelude.Structures.Quotient

import MathPrelude.Common.Integral
import MathPrelude.Common.CharZero
import MathPrelude.Common.Floating
import MathPrelude.Common.Real

-- default (Integer, Double)

-- fromInteger :: Integer -> Integer
-- fromInteger = id

------------ fun stuff
newtype Z2 = Z2 Integer deriving (Show)

instance NumEq Z2 where
	(=~) = (==)
	epsilon = Z2 0
	nearZero = (== Z2 0)

instance Eq Z2 where
	(==) (Z2 x) (Z2 y) = (x-y) `mod` 2 == 0

instance Monoid Z2 where
	mempty = Z2 0
	mappend (Z2 0) (Z2 0) = Z2 0
	mappend (Z2 1) (Z2 0) = Z2 1
	mappend (Z2 0) (Z2 1) = Z2 1
	mappend (Z2 1) (Z2 1) = Z2 0

instance Abelian Z2 where
	negate = id

instance Ring Z2 where
	one = Z2 1
	(*) (Z2 0) (Z2 0) = Z2 0
	(*) (Z2 1) (Z2 0) = Z2 0
	(*) (Z2 0) (Z2 1) = Z2 0
	(*) (Z2 1) (Z2 1) = Z2 1


-- arithmetic geometric mean
--agm :: Complex Double -> Complex Double -> Complex Double
agm :: Floating a => a -> a -> a
agm a g = fst . head . dropWhile test . iterate next $ (a,g)
	where
		next (!a,!g) = ((a+g)/ two, sqrt (a*g))
		test (!a,!g) = not $ a =~ g


ellipticK :: Floating a => a -> a
ellipticK k = (pi/2)/ agm (1-k) (1+k)
aperiod :: Double -> Complex Double
-- aperiod :: (Ord a, Field a, Floating a) => a -> Complex a Double -> Complex Double
aperiod r = (-4::Double) .* iu * (fromReal $ ellipticK (r ^ 2))
--bperiod :: (Ord a, Field a, Floating a) => a -> Complex a
bperiod :: Double -> Complex Double
bperiod r = ((-8)/(1+r)/(1+r)) .* (fromReal $ ellipticK ktilde)
	where ktilde = (1-r)*(1-r)/(1+r)/(1+r)

tau r = (aperiod r)/(bperiod r)







-- adlaj's modified agm for elliptic integrals of the 2nd kind
magm :: Floating a => a -> a -> a
magm x y = (\(x,_,_) -> x) .head . dropWhile test . iterate next $ (x,y,zero)
	where
		root (x,y,z) = sqrt ((x-z)*(y-z))
		nextx (x,y,_) = (x+y)/ 2
		nexty p@(_,_,z) = z + root p
		nextz p@(_,_,z) = z - root p
		next p = (nextx p, nexty p, nextz p)
		test (x,y,_) = not $ x =~ y

ellipticE :: Floating a => a -> a
ellipticE k = (pi/ two) * (magm one k') / (agm one k')
	where k' = sqrt $ one - k^two



