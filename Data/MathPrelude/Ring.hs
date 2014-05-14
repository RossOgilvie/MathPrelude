{-# LANGUAGE NoImplicitPrelude #-}
module Data.MathPrelude.Ring where

import BasicPrelude
import qualified Prelude as P

import Data.MathPrelude.Abelian

class Abelian a => Ring a where
	one :: a
	(*) :: a -> a -> a
	fromInteger :: Integer -> a

	one = fromInteger 1
	fromInteger n
		| n < 0       =  negate (fi (negate n))
		| otherwise   =  fi n
			where
				fi 0    =  zero
				fi 1    =  one
				fi n
					| even n    = fin + fin
					| otherwise = fin + fin + one
						where fin = fi (n `P.div` 2)
infixl 7  *

product :: Ring a => [a] -> a
product = foldr (*) one

(^) :: Ring a => a -> Int -> a
(^) x n = product $ take n $ repeat x

instance Ring Int where one = 1; (*) = (P.*)
instance Ring Integer where one = 1; (*) = (P.*)
instance Ring Int32 where one = 1; (*) = (P.*)
instance Ring Int64 where one = 1; (*) = (P.*)
instance Ring Float where one = 1; (*) = (P.*)
instance Ring Double where one = 1; (*) = (P.*)

class (Eq a, Show a, Ring a) => Num a  where
    abs, signum :: a -> a

instance Num Int where abs = P.abs; signum = P.signum
instance Num Integer where abs = P.abs; signum = P.signum
instance Num Int32 where abs = P.abs; signum = P.signum
instance Num Int64 where abs = P.abs; signum = P.signum
instance Num Float where abs = P.abs; signum = P.signum
instance Num Double where abs = P.abs; signum = P.signum

class Ring a => IntDom a where;

instance IntDom Int
instance IntDom Integer
instance IntDom Int32
instance IntDom Int64
instance IntDom Float
instance IntDom Double
