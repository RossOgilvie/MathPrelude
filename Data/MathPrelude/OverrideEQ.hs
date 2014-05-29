{-# LANGUAGE NoImplicitPrelude #-}
module Data.MathPrelude.OverrideEQ where

import BasicPrelude
import qualified Prelude as P

class NumEq a where
	(=~) :: a -> a -> Bool
	(/=~) :: a -> a -> Bool

	epsilon :: a
	nearZero :: a -> Bool

	(=~) x y = not $ (/=~) x y
	(/=~) x y = not $ (=~) x y

infixl 4  =~
infixl 4  /=~

instance NumEq Int where	(=~) = (==); epsilon = 0; nearZero = (==0)
instance NumEq Int32 where	(=~) = (==); epsilon = 0; nearZero = (==0)
instance NumEq Int64 where	(=~) = (==); epsilon = 0; nearZero = (==0)
instance NumEq Integer where	(=~) = (==); epsilon = 0; nearZero = (==0)

instance NumEq Float where (=~) x y = nearZero $ P.abs (x P.- y); epsilon = 1e-5; nearZero a = P.abs a <= epsilon
instance NumEq Double where (=~) x y = nearZero $ P.abs (x P.- y); epsilon = 1e-10; nearZero a = P.abs a <= epsilon

instance NumEq a => NumEq [a] where
	(=~) x y = length x == length y && (and $ zipWith (=~) x y)
	epsilon = [epsilon]
	nearZero xs = and . map nearZero $ xs
