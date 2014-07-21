{-# LANGUAGE NoImplicitPrelude #-}
module MathPrelude.Algebraic.EuclideanDomain
	( module MathPrelude.Algebraic.Ring
	, EuclideanDomain(..)
	, gcd, gcd'
	, extEuclidAlg
	) where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Algebraic.Ring

-----------------------------------
--- EuclideanDomain
-----------------------------------

class IntDom a => EuclideanDomain a where
	stdAssociate    :: a -> a
	stdUnit         :: a -> a
	normalize       :: a -> (a, a)

	div, mod        :: a -> a -> a
	divMod          :: a -> a -> (a,a)

	-- Minimal complete definition:
	-- (stdUnit or normalize) and (divMod or (div and mod))
	stdAssociate x  =  x `div` stdUnit x
	stdUnit x       =  snd (normalize x)
	normalize x     =  (stdAssociate x, stdUnit x)

	n `divMod` d    =  (n `div` d, n `mod` d)
	n `div` d       =  fst $ divMod n d
	n `mod` d       =  snd $ divMod n d


-----------------------------------
--- Methods
-----------------------------------

gcd :: EuclideanDomain a => a -> a -> a
gcd a b = stdAssociate $ gcd' a b
gcd' a b
	| a >>~ b = a
	| otherwise = gcd' b (a `mod` b)

extEuclidAlg :: (EuclideanDomain a, NumEq a) => a -> a -> (a,a)
extEuclidAlg a b
	| b >>~ r = (zero,one)
	| otherwise = (y, x - (y * q))
		where
			(q,r) = a `divMod` b
			(x,y) = extEuclidAlg b r


-----------------------------------
--- Instances
-----------------------------------
instance EuclideanDomain Integer where stdAssociate 	= P.abs; stdUnit = P.signum; divMod = P.divMod
instance EuclideanDomain Int where stdAssociate 	= P.abs;stdUnit = P.signum;	divMod = P.divMod;
instance EuclideanDomain Int32 where stdAssociate 	= P.abs; stdUnit = P.signum; divMod = P.divMod
instance EuclideanDomain Int64 where stdAssociate 	= P.abs; stdUnit = P.signum; divMod = P.divMod

instance EuclideanDomain a => EuclideanDomain (Maybe a) where
	stdAssociate = liftM stdAssociate
	stdUnit = liftM stdUnit
	div = liftM2 div
	mod = liftM2 mod