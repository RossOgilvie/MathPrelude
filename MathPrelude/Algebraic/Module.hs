{-# LANGUAGE RebindableSyntax, UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances #-}
module MathPrelude.Algebraic.Module where

import BasicPrelude

import MathPrelude.Algebraic.Ring
import MathPrelude.Algebraic.Group
import MathPrelude.Algebraic.Field

-----------------------------------
--- Module
-----------------------------------

-- | A module over a ring is a generalisation of a vector space over a field. In fact the generalisation is exactly to allow ring. Scaling must distribute over addition of both the ring and the module. I am not concerned with left versus right modules, so both scalings are available and should be equal.
class (Abelian m, Ring s) ⇒ Module m s where
	-- | Scale a vector by a ring element.
	scale ∷ s → m → m
	-- | Scale a vector by a ring element on the left.
	(.*) ∷ s → m → m
	-- | Scale a vector by a ring element on the right.
	(*.) ∷ m → s → m

	(.*) = scale
	(*.) = flip (.*)
	scale = flip (*.)

	{-# MINIMAL scale | (.*) | (*.) #-}

-----------------------------------
--- Instances
-----------------------------------

instance Ring r ⇒ Module r r where
  scale r s = r*s

-- instance Ring r ⇒ Module r Integer where
-- 	scale n r = fromInteger n * r

-----------------------------------
--- Vector Space
-----------------------------------

-- class (Abelian v, Field k) ⇒ VectorSpace v k

-----------------------------------
--- Instances
-----------------------------------

-- instance Module v k ⇒ VectorSpace v k

-----------------------------------
--- Methods
-----------------------------------
-- | In the case where the base ring is actually a field after all, scalar division makes sense.
(./) ∷ (Field k, Module v k) ⇒ k → v → v
(./) k v = scale (recip k) v
-- | In the case where the base ring is actually a field after all, scalar division makes sense.
(/.) ∷ (Field k, Module v k) ⇒ v → k → v
(/.) = flip (./)
