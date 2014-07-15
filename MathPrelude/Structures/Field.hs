{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Structures.Field
	( module MathPrelude.Structures.Ring
	, Field(..)
	, half
	)  where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Ring

------------------------------
--- Classes
------------------------------

class IntDom a => Field a where
	recip :: a -> a
	(/) :: a -> a -> a

	recip x = one / x
	(/) x y = x * recip y


------------------------------
--- Methods
------------------------------

half :: Field a => a
half = recip 2

------------------------------
--- Instances
------------------------------
instance Field Float where recip = P.recip; (/) = (P./)
instance Field Double where recip = P.recip; (/) = (P./)
