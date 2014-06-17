{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Common.CharZero
	( Rational
	, CharZero(..)
	, Q(..)
	, Fractional(..)
	, fromRational
	, fromRational98
	, toRational98
	) where

------------------------------
--- Imports
------------------------------
import BasicPrelude
import qualified Prelude as P
import qualified Data.Ratio as Ratio98

import MathPrelude.Structures.Quotient
import MathPrelude.Common.Integral

------------------------------
--- Classes
------------------------------
type Rational = Quotient Integer

class CharZero a where
	fromRational' :: Rational -> a

class CharZero a => Q a where
	toRational :: a -> Rational
	convRational :: a -> a
	convRational = fromRational' . toRational

class CharZero a => Fractional a

------------------------------
--- Methods
------------------------------
fromRational :: CharZero a => P.Rational -> a
fromRational x = fromRational' (Ratio98.numerator x :% Ratio98.denominator x)

toRational98 :: Rational -> P.Rational
toRational98 x = numerator x Ratio98.% denominator x
fromRational98 :: P.Rational -> Rational
fromRational98 x = Ratio98.numerator x :% Ratio98.denominator x

------------------------------
--- Instances
------------------------------
instance Integral a => CharZero (Quotient a) where
	 fromRational' (x :% y) = (fromInteger x) :% (fromInteger y)
instance Integral a => Q (Quotient a) where
	 toRational (x :% y) = (toInteger x) :% (toInteger y)


instance CharZero Float where	fromRational' = P.fromRational . toRational98
instance CharZero Double where fromRational' = P.fromRational . toRational98

instance Q Float where toRational = fromRational98 . P.toRational
instance Q Double where toRational = fromRational98 . P.toRational

instance Fractional Float
instance Fractional Double
