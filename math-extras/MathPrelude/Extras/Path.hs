{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE UnicodeSyntax         #-}
module MathPrelude.Extras.Path
  ( Path(..)
  , pathIntegration
  ) where

-----------------------------------
--- Imports
-----------------------------------


-- from math-prelude
import MathPrelude
import MathPrelude.Classes.Field
import MathPrelude.Classes.VectorSpace
import MathPrelude.Classes.Rational
import MathPrelude.Classes.Transcendental
import MathPrelude.Constructions.Complex

-- from math-calculus
import MathPrelude.Calculus.Integration


-----------------------------------
--- Data
-----------------------------------

-- | A path in a space (the type of type-parameter), defined on the interval [start,end].
data Path a = Path {path ∷ Double → a, start ∷ Double, end ∷ Double}


-----------------------------------
--- Methods
-----------------------------------

-- | Use numerical integration to integrate a function over the given path.
pathIntegration ∷ (Field b, Module b Double, Approx b) ⇒ (a → b) → Path a → b
pathIntegration f g = numIntegrate (f. path g) (start g) (end g)

-- | Use path integration around a point to calculate a residue. Currently BROKEN because of branch cuts.
residue f z = pathIntegration f g
  where g = Path (\t → z + fromPolar 1e-3 t) (-pi) (pi)
