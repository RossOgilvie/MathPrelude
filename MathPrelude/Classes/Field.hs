{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
-- | A field is a ring where all non-zero elements have multiplicative inverses. Ie, it has a division operation.
module MathPrelude.Classes.Field
	( module MathPrelude.Classes.Ring
	, Field(..)
	, half
	)  where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Classes.Ring

------------------------------
--- Classes
------------------------------

-- | A field is an integral domain where every non zero element has a multiplicative inverse (a reciprocal). Behaviour is unspeicifed if you divide by zero.
class IntDom a ⇒ Field a where
	-- | The reciprocal of an element
	recip ∷ a → a
	-- | Divide one element by another
	(/) ∷ a → a → a

	recip x = one / x
	(/) x y = x * recip y

	{-# MINIMAL recip | (/) #-}


------------------------------
--- Methods
------------------------------

-- | A convenient helper. half = 0.5 = 1/2 = recip 2
half ∷ Field a ⇒ a
half = recip 2

------------------------------
--- Instances
------------------------------
instance Field Float where recip = P.recip; (/) = (P./)
instance Field Double where recip = P.recip; (/) = (P./)

instance Field b ⇒ Field (a → b) where
	recip f = recip . f
	(/) f g x = f x / g x
