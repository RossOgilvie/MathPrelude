{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Instances.Q where

import BasicPrelude
import qualified Prelude as P
import qualified Data.Ratio as Ratio98

import MathPrelude.Structures.Quotient
import MathPrelude.Instances.Z
import MathPrelude.Representations.PreludeNumConst

class Q a where
	fromRational' :: Rational -> a
	toRational :: a -> Rational
	convRational :: a -> a
	convRational = fromRational' . toRational

fromRational :: Q a => P.Rational -> a
fromRational x = fromRational' (Ratio98.numerator x :% Ratio98.denominator x)

class Q a => Fractional a

type Rational = Quotient Integer

instance Z a => Q (Quotient a) where
	 fromRational' (x :% y) = (fromInteger x) :% (fromInteger y)
	 toRational (x :% y) = (toInteger x) :% (toInteger y)
