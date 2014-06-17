{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Common.Floating where

------------------------------
--- Imports
------------------------------
import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Field

------------------------------
--- Classes
------------------------------
class Field a => Floating a where
	pi                  :: a
	exp, log, sqrt      :: a -> a
	(**), logBase       :: a -> a -> a
	sin, cos, tan       :: a -> a
	asin, acos, atan    :: a -> a
	atan2								:: a -> a -> a
	sinh, cosh, tanh    :: a -> a
	asinh, acosh, atanh :: a -> a

	logBase b x = log x / log b
	x ** y = exp ( y * log x )
	tan x = sin x / cos x
	sinh x = (exp x - exp (negate x)) / two
	cosh x = (exp x + exp (negate x)) / two
	tanh x = sinh x / cosh x
	asinh x = log (x + sqrt(x*x  + one))
	acosh x = log (x + sqrt(x*x  - one))
	atanh x = log ((one + x)/(one - x)) / two

------------------------------
--- Instances
------------------------------
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
