{-# LANGUAGE RebindableSyntax, UnicodeSyntax, MultiParamTypeClasses, FlexibleContexts #-}
module MathPrelude.Extras.Path
  ( Path(..)
  , pathIntegration
  ) where

import BasicPrelude
import MathPrelude.Algebraic.Field
import MathPrelude.Algebraic.Module
import MathPrelude.Extras.Integration
import MathPrelude.Constructions.Complex
import MathPrelude.Common.Rational
import MathPrelude.Common.Transcendental

-- | A path in a space (the type of type-parameter), defined on the interval [start,end].
data Path a = Path {path ∷ Double → a, start ∷ Double, end ∷ Double}

-- | Use numerical integration to integrate a function over the given path.
pathIntegration ∷ (Field b, Module b Double, NumEq b) ⇒ (a → b) → Path a → b
pathIntegration f g = numIntegrate (f. path g) (start g) (end g)

-- | Use path integration around a point to calculate a residue. Currently BROKEN because of branch cuts.
residue f z = pathIntegration f g
  where g = Path (\t → z + fromPolar 1e-3 t) (-pi) (pi)
