{-# LANGUAGE NoImplicitPrelude #-}
module MathPrelude.Structures.Field
	( module MathPrelude.Structures.Ring
	, Field(..)
	, Floating(..)
	)  where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Ring

------------------------------
--- Field
------------------------------

class IntDom a => Field a where
	recip :: a -> a
	(/) :: a -> a -> a

	recip x = one / x
	(/) x y = x * recip y

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

