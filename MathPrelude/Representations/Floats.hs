{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Representations.Floats where

import BasicPrelude
import qualified Prelude as P
-- import qualified GHC.Real as GHC
import qualified Data.Ratio as Ratio98

import MathPrelude.Instances.Z
import MathPrelude.Instances.Q
import MathPrelude.Instances.R

import MathPrelude.Structures.Quotient
import MathPrelude.Representations.PreludeNumConst

toRational98 :: Rational -> P.Rational
toRational98 x = numerator x Ratio98.% denominator x
fromRational98 :: P.Rational -> Rational
fromRational98 x = Ratio98.numerator x :% Ratio98.denominator x


instance Monoid Float where mempty = zeroFloat; mappend = (P.+)
instance Abelian Float where zero = mempty; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Ring Float where one = oneFloat; (*) = (P.*);
instance IntDom Float
instance Field Float where recip = P.recip; (/) = (P./)

instance Z Float where
	fromInteger = P.fromInteger
	toInteger = P.round
instance Q Float where
	fromRational' = P.fromRational . toRational98
--  	toRational x = x :% 1.0

instance Num Float where abs = P.abs; signum = P.signum
instance Floating Float where
	pi = P.pi
	exp = P.exp
	sqrt = P.sqrt
	log = P.log
	(**) = (P.**)
	logBase = P.logBase
	sin = P.sin
	cos = P.cos
	tan = P.tan
	asin = P.asin
	acos = P.acos
	atan = P.atan
	atan2 = P.atan2
	sinh = P.sinh
	cosh = P.cosh
	tanh = P.tanh
	asinh = P.asinh
	acosh = P.acosh
	atanh = P.atanh




instance Monoid Double where mempty = zeroDouble; mappend = (P.+)
instance Abelian Double where zero = mempty; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Ring Double where one = oneDouble; (*) = (P.*);
instance IntDom Double
instance Field Double where recip = P.recip; (/) = (P./)

instance Z Double where
	fromInteger = P.fromInteger
	toInteger = P.round
instance Q Double where
	fromRational' = P.fromRational . toRational98
-- 	toRational = dumb . P.toRational

instance Num Double where abs = P.abs; signum = P.signum
instance Floating Double where
	pi = P.pi
	exp = P.exp
	sqrt = P.sqrt
	log = P.log
	(**) = (P.**)
	logBase = P.logBase
	sin = P.sin
	cos = P.cos
	tan = P.tan
	asin = P.asin
	acos = P.acos
	atan = P.atan
	atan2 = P.atan2
	sinh = P.sinh
	cosh = P.cosh
	tanh = P.tanh
	asinh = P.asinh
	acosh = P.acosh
	atanh = P.atanh
