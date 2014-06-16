{-# LANGUAGE NoImplicitPrelude #-}
module Data.MathPrelude.Ring(module Data.MathPrelude.Abelian, Ring(..), (^), product, Num(..), IntDom(..), Integral(..), ifThenElse) where

import BasicPrelude
import qualified Prelude as P

import Data.MathPrelude.Abelian
import Data.MathPrelude.PreludeNumConst

class Abelian a => Ring a where
	one :: a
	(*) :: a -> a -> a
	fromInteger :: Integer -> a

	one = fromInteger oneInteger
	fromInteger n
		| n < zeroInteger = negate (fi (negate n))
		| otherwise = fi n
			where
				fi n
					| n =~ zeroInteger = zero
					| n =~ oneInteger = one
					| even n    = fin + fin
					| otherwise = fin + fin + one
						where fin = fi (n `P.div` twoInteger)

infixl 7 *

product :: Ring a => [a] -> a
product = foldr (*) one

(^) :: Ring a => a -> Int -> a
(^) x n = product $ take n $ repeat x

instance Ring Int where one = oneInt; (*) = (P.*); fromInteger = P.fromInteger
instance Ring Integer where one = oneInteger; (*) = (P.*); fromInteger = P.fromInteger
instance Ring Int32 where one = oneInt32; (*) = (P.*); fromInteger = P.fromInteger
instance Ring Int64 where one = oneInt64; (*) = (P.*); fromInteger = P.fromInteger
instance Ring Float where one = oneFloat; (*) = (P.*); fromInteger = P.fromInteger
instance Ring Double where one = oneDouble; (*) = (P.*); fromInteger = P.fromInteger

class (Eq a, Show a, Ring a) => Num a  where
    abs, signum :: a -> a

instance Num Int where abs = P.abs; signum = P.signum
instance Num Integer where abs = P.abs; signum = P.signum
instance Num Int32 where abs = P.abs; signum = P.signum
instance Num Int64 where abs = P.abs; signum = P.signum
instance Num Float where abs = P.abs; signum = P.signum
instance Num Double where abs = P.abs; signum = P.signum

class Ring a => IntDom a

instance IntDom Int
instance IntDom Integer
instance IntDom Int32
instance IntDom Int64
instance IntDom Float
instance IntDom Double

class Ring a => Integral a where
	convIntegral :: a -> a
	toIntegral :: Integer -> a
	fromIntegral :: a -> Integer

	convIntegral =  toIntegral . fromIntegral

instance Integral Int where toIntegral = P.fromInteger; fromIntegral = P.toInteger
instance Integral Integer where toIntegral = P.fromInteger; fromIntegral = P.toInteger
instance Integral Int32 where toIntegral = P.fromInteger; fromIntegral = P.toInteger
instance Integral Int64 where toIntegral = P.fromInteger; fromIntegral = P.toInteger











