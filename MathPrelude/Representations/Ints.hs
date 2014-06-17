{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Representations.Ints where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Instances.Z

import MathPrelude.Structures.EuclideanDomain
import MathPrelude.Representations.PreludeNumConst




instance Monoid Integer where mempty = zeroInteger; mappend = (P.+)
instance Abelian Integer where zero = mempty; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Ring Integer where one = oneInteger; (*) = (P.*);
instance IntDom Integer
instance EuclideanDomain Integer where stdAssociate 	= P.abs; stdUnit = P.signum; divMod = P.divMod

instance Z Integer where fromInteger = id; toInteger = id

instance Num Integer where abs = P.abs; signum = P.signum
instance Integral Integer






instance Monoid Int where mempty = zeroInt; mappend = (P.+)
instance Abelian Int where zero = mempty; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Ring Int where one = oneInt; (*) = (P.*);
instance IntDom Int
instance EuclideanDomain Int where stdAssociate 	= P.abs;stdUnit = P.signum;	divMod = P.divMod;

instance Z Int where fromInteger = P.fromInteger; toInteger = P.toInteger

instance Num Int where abs = P.abs; signum = P.signum
instance Integral Int



instance Monoid Int32 where mempty = zeroInt32; mappend = (P.+)
instance Abelian Int32 where zero = mempty; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Ring Int32 where one = oneInt32; (*) = (P.*);
instance IntDom Int32
instance EuclideanDomain Int32 where stdAssociate 	= P.abs; stdUnit = P.signum; divMod = P.divMod

instance Z Int32 where fromInteger = P.fromInteger; toInteger = P.toInteger

instance Num Int32 where abs = P.abs; signum = P.signum
instance Integral Int32




instance Monoid Int64 where mempty = zeroInt64; mappend = (P.+)
instance Abelian Int64 where zero = mempty; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Ring Int64 where one = oneInt64; (*) = (P.*);
instance IntDom Int64
instance EuclideanDomain Int64 where stdAssociate 	= P.abs; stdUnit = P.signum; divMod = P.divMod

instance Z Int64 where fromInteger = P.fromInteger; toInteger = P.toInteger

instance Num Int64 where abs = P.abs; signum = P.signum
instance Integral Int64





-- fromInteger n
-- 		| n < zeroInteger = negate (fi (negate n))
-- 		| otherwise = fi n
-- 			where
-- 				fi n
-- 					| n =~ zeroInteger = zero
-- 					| n =~ oneInteger = one
-- 					| even n    = fin + fin
-- 					| otherwise = fin + fin + one
-- 						where fin = fi (n `P.div` twoInteger)
-- toInteger n
-- 		| n < zero = negate (fi (negate n))
-- 		| otherwise = fi n
-- 			where
-- 				fi :: (Ord a, EuclideanDomain a) => a -> Integer
-- 				fi n
-- 					| n =~ zero = zeroInteger
-- 					| n =~ one = oneInteger
-- 					| r =~ zero    = fin + fin
-- 					| otherwise = fin + fin + oneInteger
-- 						where
-- 							(q,r) = divMod n two
-- 							fin = fi q
