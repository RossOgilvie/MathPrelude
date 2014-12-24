{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE UnicodeSyntax          #-}

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
-- | A module (the algebraic object, not the Haskell file) abstracts the notion of scaling an additive group. A vector space is a module where the scalars form a field.
module MathPrelude.Classes.Module where

import           BasicPrelude

import           MathPrelude.Classes.Field
import           MathPrelude.Classes.Norm

-----------------------------------
--- Module
-----------------------------------

-- | A module over a ring is a generalisation of a vector space over a field. In fact the generalisation is exactly to allow ring. Scaling must distribute over addition of both the ring and the module. I am not concerned with left versus right modules, so both scalings are available and should be equal.
class (Abelian m, Ring s) ⇒ Module m s | m → s where
	-- | Scale a vector by a ring element.
	scale ∷ s → m → m
	{-# MINIMAL scale #-}

-- | Scale a vector by a ring element on the left.
(.*) ∷ Module m s ⇒ s → m → m
(.*) = scale
-- | Scale a vector by a ring element on the right.
(*.) ∷ Module m s ⇒ m → s → m
(*.) = flip scale

-- | A vector space is a set of vectors that can be added and scaled by an element of the 'base' field. All (finite rank) vector spaces are isomorphic to a power of the field, and so can be dealt with entirely in terms of bases, but this is not explored/exploited here (yet).
class (Module m s, Field s) ⇒ VectorSpace m s

-----------------------------------
--- Instances
-----------------------------------

-----------------------------------
--- Methods
-----------------------------------
-- | In the case where the base ring is actually a field after all, scalar division makes sense.
(./) ∷ (Field k, Module v k) ⇒ k → v → v
(./) k = scale (recip k)
-- | In the case where the base ring is actually a field after all, scalar division makes sense.
(/.) ∷ (Field k, Module v k) ⇒ v → k → v
(/.) = flip (./)
-- | Scale a vector by dividing by its norm. This should make it uni length.
normalise ∷ (VectorSpace v s, Norm v s) ⇒ v → v
normalise v = norm v ./ v
