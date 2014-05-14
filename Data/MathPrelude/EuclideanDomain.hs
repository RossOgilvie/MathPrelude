{-# LANGUAGE NoImplicitPrelude #-}
module Data.MathPrelude.EuclideanDomain where

import BasicPrelude
import qualified Prelude as P

import Data.MathPrelude.Ring
import Data.MathPrelude.Abelian

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
	n `div` d       =  q  where (q,r) = divMod n d
	n `mod` d       =  r  where (q,r) = divMod n d

-----------------------------------
--- Instances
-----------------------------------

instance EuclideanDomain Integer where
	stdAssociate 	= P.abs
	stdUnit         = P.signum
	divMod          = P.divMod

instance EuclideanDomain Int where
	stdAssociate 	= P.abs
	stdUnit         = P.signum
	divMod          = P.divMod

instance EuclideanDomain Int32 where
	stdAssociate 	= P.abs
	stdUnit         = P.signum
	divMod          = P.divMod

instance EuclideanDomain Int64 where
	stdAssociate 	= P.abs
	stdUnit         = P.signum
	divMod          = P.divMod

-----------------------------------
--- Methods
-----------------------------------

gcd :: EuclideanDomain a => a -> a -> a
gcd a b
	| b == zero = a
	| otherwise = gcd b (a `mod` b)