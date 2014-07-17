{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Common.Transcendental where

------------------------------
--- Imports
------------------------------
import BasicPrelude
import qualified Prelude as P

import MathPrelude.Algebraic.Field

------------------------------
--- Classes
------------------------------
class Field a => Transcendental a where
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
	sinh x = (exp x - exp (negate x)) / 2
	cosh x = (exp x + exp (negate x)) / 2
	tanh x = sinh x / cosh x
	asinh x = log (x + sqrt(x*x  + 1))
	acosh x = log (x + sqrt(x*x  - 1))
	atanh x = log ((1 + x)/(1 - x)) / 2

------------------------------
--- Instances
------------------------------
instance Transcendental Float where
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

instance Transcendental Double where
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
