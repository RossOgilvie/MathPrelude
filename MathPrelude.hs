-- {-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}

-- | A standard collections of modules covering the most common areas of mathematics/arithmetic.
module MathPrelude ( module X ) where

-- BasicPrelude is imported via IntegralLists, which subs in list functions
-- import BasicPrelude as X
import IntegralLists as X

import MathPrelude.Classes.NumEq as X

import MathPrelude.Classes.Logic as X
import MathPrelude.Classes.Group as X
import MathPrelude.Classes.Ring as X
import MathPrelude.Classes.EuclideanDomain as X
import MathPrelude.Classes.Field as X

import MathPrelude.Classes.Integral as X
import MathPrelude.Classes.Rational as X
import MathPrelude.Classes.Real as X
import MathPrelude.Classes.Transcendental as X

import MathPrelude.Constructions.Ratio as X
