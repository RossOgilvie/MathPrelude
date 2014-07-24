-- {-# LANGUAGE RebindableSyntax, #-}
module MathPrelude
	( module BasicPrelude
	, module MathPrelude.Classes.NumEq

	, module MathPrelude.Algebraic.Logic
	, module MathPrelude.Algebraic.Group
	, module MathPrelude.Algebraic.Ring
	, module MathPrelude.Algebraic.EuclideanDomain
	, module MathPrelude.Algebraic.Field

	, module MathPrelude.Constructions.Ratio

	, module MathPrelude.Common.Integral
	, module MathPrelude.Common.Rational
	, module MathPrelude.Common.Transcendental
	, module MathPrelude.Common.Real
	) where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Classes.NumEq
import MathPrelude.Classes.Derivation
import MathPrelude.Classes.Evaluable
import MathPrelude.Classes.Norm

import MathPrelude.Algebraic.Logic
import MathPrelude.Algebraic.Group
import MathPrelude.Algebraic.Ring
import MathPrelude.Algebraic.EuclideanDomain
import MathPrelude.Algebraic.Field
import MathPrelude.Algebraic.Module

import MathPrelude.Constructions.Complex
import MathPrelude.Constructions.ContinuedFractions
import MathPrelude.Constructions.Polynomial
import MathPrelude.Constructions.PowerSeries
import MathPrelude.Constructions.Projective
import MathPrelude.Constructions.Quotient
import MathPrelude.Constructions.Ratio

import MathPrelude.Common.Integral
import MathPrelude.Common.Convergence
import MathPrelude.Common.PreludeNumConst
import MathPrelude.Common.Rational
import MathPrelude.Common.Real
import MathPrelude.Common.Transcendental

import MathPrelude.Extras.Combinatorics
import MathPrelude.Extras.DifferentialEqns
import MathPrelude.Extras.EllipticFunctions
import MathPrelude.Extras.Integration
import MathPrelude.Extras.NewtonsMethod
import MathPrelude.Extras.NumberRings
import MathPrelude.Extras.Path
import MathPrelude.Extras.PolynomialFactorisation
import MathPrelude.Extras.Primes
import MathPrelude.Extras.SpecialFunc
