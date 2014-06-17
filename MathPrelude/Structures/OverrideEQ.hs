{-# LANGUAGE NoImplicitPrelude #-}
module MathPrelude.Structures.OverrideEQ where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Representations.PreludeNumConst

class NumEq a where
	(=~) :: a -> a -> Bool
	(/=~) :: a -> a -> Bool

	epsilon :: a
	nearZero :: a -> Bool

	(=~) x y = not $ (/=~) x y
	(/=~) x y = not $ (=~) x y

infixl 4  =~
infixl 4  /=~

instance NumEq Int where	(=~) = (==); epsilon = zeroInt; nearZero = (==zeroInt)
instance NumEq Int32 where	(=~) = (==); epsilon = zeroInt32; nearZero = (==zeroInt32)
instance NumEq Int64 where	(=~) = (==); epsilon = zeroInt64; nearZero = (==zeroInt64)
instance NumEq Integer where	(=~) = (==); epsilon = zeroInteger; nearZero = (==zeroInteger)

instance NumEq Float where (=~) x y = nearZero $ P.abs (x P.- y); epsilon = f1em5; nearZero a = P.abs a <= epsilon
instance NumEq Double where (=~) x y = nearZero $ P.abs (x P.- y); epsilon = d1em10; nearZero a = P.abs a <= epsilon

instance NumEq a => NumEq [a] where
	(=~) x y = length x == length y && (and $ zipWith (=~) x y)
	epsilon = [epsilon]
	nearZero xs = and . map nearZero $ xs

instance (NumEq a, NumEq b) => NumEq (a,b) where
	(a,b) =~ (c,d) = a =~ c && b =~ d
	epsilon = (epsilon,epsilon)
	nearZero (a,b) = nearZero a && nearZero b
