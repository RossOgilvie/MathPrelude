{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances, BangPatterns#-}
module MathPrelude
	( module MathPrelude
	, module BasicPrelude
	)where

import BasicPrelude
import qualified Prelude as P

import Data.MathPrelude.Monoid
import Data.MathPrelude.Abelian
import Data.MathPrelude.Ring
import Data.MathPrelude.Field
import Data.MathPrelude.EuclideanDomain
import Data.MathPrelude.Polynomial
import Data.MathPrelude.Complex
import Data.MathPrelude.Module



------------ fun stuff
newtype Z2 = Z2 Integer deriving Show

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
agm :: (Field a, Floating a) => a -> a -> a
agm a g = fst . head . dropWhile test . iterate next $ (a,g)
	where
		next (!a,!g) = ((a+g)/ fromInteger 2, sqrt (a*g))
		test (!a,!g) = not $ eqFloat a g


ellipticK ::(Field a, Floating a) => a -> a
ellipticK k = (pi/fromInteger 2)/ agm (one-k) (one+k)
aperiod :: Double -> Complex Double
-- aperiod :: (Ord a, Field a, Floating a) => a -> Complex a Double -> Complex Double
aperiod r = (-4::Double) .* iu * (fromReal $ ellipticK (r ^ 2))
--bperiod :: (Ord a, Field a, Floating a) => a -> Complex a
bperiod :: Double -> Complex Double
bperiod r = ((-8)/(1+r)/(1+r)) .* (fromReal $ ellipticK ktilde)
	where ktilde = (1-r)*(1-r)/(1+r)/(1+r)

tau r = (aperiod r)/(bperiod r)







-- adlaj's modified agm for elliptic integrals of the 2nd kind
magm :: (Field a, Floating a) => a -> a -> a
magm x y = (\(x,_,_) -> x) .head . dropWhile test . iterate next $ (x,y,zero)
	where
		root (x,y,z) = sqrt ((x-z)*(y-z))
		nextx (x,y,_) = (x+y)/ fromInteger 2
		nexty p@(_,_,z) = z + root p
		nextz p@(_,_,z) = z - root p
		next p = (nextx p, nexty p, nextz p)
		test (x,y,_) = not $ eqFloat x y

ellipticE :: (Field a, Floating a) => a -> a
ellipticE k = (pi/fromInteger 2) * (magm one k') / (agm one (sqrt k'))
	where k' = one - k*k



