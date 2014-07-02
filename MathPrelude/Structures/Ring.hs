{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
module MathPrelude.Structures.Ring
	( module MathPrelude.Structures.Abelian
	, Ring(..)
	, Num(..)
	, IntDom(..)
	, (^)
	, product
	, two
	, ifThenElse
	) where

-----------------------------------
--- Imports
-----------------------------------
import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Abelian
import MathPrelude.Common.PreludeNumConst


-----------------------------------
--- Classes
-----------------------------------
class Abelian a => Ring a where
	one :: a
	(*) :: a -> a -> a
	fromInteger :: Integer -> a

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


class (Eq a, Show a, Ring a) => Num a  where
    abs, signum :: a -> a

-- instance Num a => P.Num a

class Ring a => IntDom a


-----------------------------------
--- Methods
-----------------------------------
product :: Ring a => [a] -> a
product = foldr (*) one

(^) :: Ring a => a -> Int -> a
(^) x n
	| n < 0 = error "negative power"
	| n == 0 = one
	| otherwise = product $ zipWith f (intToBinary n) powers
		where
			powers = iterate (\a -> a*a) x
			f True x = x
			f False _ = one


intToBinary :: Int -> [Bool]
intToBinary n = reverse $ intToBinary' n powers
	where
		powers = reverse $ takeWhile (<= n) twopowers

intToBinary' _ [] = []
intToBinary' n (x:xs)
	| n >= x = True : intToBinary' (n-x) xs
	| otherwise = False : intToBinary' n xs

twopowers = 1 : map (*2) twopowers



two :: Ring a => a
two = one + one

-----------------------------------
--- Instances
-----------------------------------

instance Ring Integer where one = oneInteger; (*) = (P.*); fromInteger = P.fromInteger;
instance Ring Int where one = oneInt; (*) = (P.*); fromInteger = P.fromInteger;
instance Ring Int32 where one = oneInt32; (*) = (P.*); fromInteger = P.fromInteger;
instance Ring Int64 where one = oneInt64; (*) = (P.*); fromInteger = P.fromInteger;
instance Ring Float where one = oneFloat; (*) = (P.*); fromInteger = P.fromInteger;
instance Ring Double where one = oneDouble; (*) = (P.*); fromInteger = P.fromInteger;

instance IntDom Integer
instance IntDom Int
instance IntDom Int32
instance IntDom Int64
instance IntDom Float
instance IntDom Double

instance Num Integer where abs = P.abs; signum = P.signum
instance Num Int where abs = P.abs; signum = P.signum
instance Num Int32 where abs = P.abs; signum = P.signum
instance Num Int64 where abs = P.abs; signum = P.signum
instance Num Float where abs = P.abs; signum = P.signum
instance Num Double where abs = P.abs; signum = P.signum

instance Ring a => Ring (Maybe a) where one = Just one; (*) = liftM2 (*); fromInteger x = Just (fromInteger x)
instance IntDom a => IntDom (Maybe a)
