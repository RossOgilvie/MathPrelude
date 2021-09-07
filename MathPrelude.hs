-- | A standard collections of modules covering the most common areas of mathematics/arithmetic.
module MathPrelude ( module X ) where

import MathPrelude.Prelude.CorePrelude as X
import MathPrelude.Prelude.IntegralLists as X

import MathPrelude.Classes.Group as X
import MathPrelude.Classes.Ring as X
import MathPrelude.Classes.EuclideanDomain as X
import MathPrelude.Classes.Field as X

import MathPrelude.Classes.Integral as X
import MathPrelude.Classes.Rational as X
import MathPrelude.Classes.Real as X

import MathPrelude.Classes.Norm as X
import MathPrelude.Classes.VectorSpace as X

import MathPrelude.Classes.Approximate as X
import MathPrelude.Classes.Transcendental as X

import MathPrelude.Constructions.Ratio as X
