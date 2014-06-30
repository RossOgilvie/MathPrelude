{-# LANGUAGE RebindableSyntax, MultiParamTypeClasses, FlexibleInstances, BangPatterns#-}
module MathPrelude
	( module BasicPrelude
	, module MathPrelude.Structures.Ratio
	, module MathPrelude.Structures.Module
	, module MathPrelude.Common.Integral
	, module MathPrelude.Common.CharZero
	, module MathPrelude.Common.Transcendental
	, module MathPrelude.Common.Real
	) where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Polynomial
import MathPrelude.Structures.Complex
import MathPrelude.Structures.Module
import MathPrelude.Structures.Ratio
import MathPrelude.Structures.Quotient

import MathPrelude.Common.Integral
import MathPrelude.Common.CharZero
import MathPrelude.Common.Transcendental
import MathPrelude.Common.Real


------------ fun stuff
type Z2 = Quotient Integer
z2 z = proj 2 z

type Z3 = Quotient Integer
z3 z = proj 3 z


-- arithmetic geometric mean
--agm :: Complex Double -> Complex Double -> Complex Double
agm :: Transcendental a => a -> a -> a
agm a g = fst . head . dropWhile test . iterate next $ (a,g)
	where
		next (!a,!g) = ((a+g)/ two, sqrt (a*g))
		test (!a,!g) = not $ a =~ g


ellipticK :: Transcendental a => a -> a
ellipticK k = (pi/2)/ agm (1-k) (1+k)
aperiod :: Double -> Complex Double
-- aperiod :: (Ord a, Field a, Transcendental a) => a -> Complex a Double -> Complex Double
aperiod r = (-4::Double) .* iu * (fromReal $ ellipticK (r ^ 2))
--bperiod :: (Ord a, Field a, Transcendental a) => a -> Complex a
bperiod :: Double -> Complex Double
bperiod r = ((-8)/(1+r)/(1+r)) .* (fromReal $ ellipticK ktilde)
	where ktilde = (1-r)*(1-r)/(1+r)/(1+r)

tau r = (aperiod r)/(bperiod r)







-- adlaj's modified agm for elliptic integrals of the 2nd kind
magm :: Transcendental a => a -> a -> a
magm x y = (\(x,_,_) -> x) .head . dropWhile test . iterate next $ (x,y,zero)
	where
		root (x,y,z) = sqrt ((x-z)*(y-z))
		nextx (x,y,_) = (x+y)/ 2
		nexty p@(_,_,z) = z + root p
		nextz p@(_,_,z) = z - root p
		next p = (nextx p, nexty p, nextz p)
		test (x,y,_) = not $ x =~ y

ellipticE :: Transcendental a => a -> a
ellipticE k = (pi/ two) * (magm one k') / (agm one k')
	where k' = sqrt $ one - k^two



---------------- test poly quotient rings
type Coms = Quotient (Poly Double)
irred :: Poly Double
irred = poly [1,0,1]

comProj = proj irred

i :: Coms
i = comProj $ poly [0,1]
