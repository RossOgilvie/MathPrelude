-- {-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}

-- | Expport some useful modules for working with polynomials, such as factorisation and complex numbers.
module MathPrelude.Exports.Polynomials( module X ) where

import MathPrelude.Classes.Module as X

import MathPrelude.Constructions.Complex as X
import MathPrelude.Constructions.Polynomial as X

import MathPrelude.Extras.PolynomialFactorisation as X
