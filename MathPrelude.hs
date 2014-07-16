-- {-# LANGUAGE RebindableSyntax, #-}
module MathPrelude
	( module BasicPrelude
	, module MathPrelude.Structures.Logic
	, module MathPrelude.Structures.NumEq

	, module MathPrelude.Structures.Abelian
	, module MathPrelude.Structures.Ring
	, module MathPrelude.Structures.EuclideanDomain
	, module MathPrelude.Structures.Field
	-- , module MathPrelude.Structures.Module

	-- , module MathPrelude.Structures.Polynomial
	-- , module MathPrelude.Structures.Complex
	, module MathPrelude.Structures.Ratio
	-- , module MathPrelude.Structures.Quotient

	, module MathPrelude.Common.Integral
	, module MathPrelude.Common.CharZero
	, module MathPrelude.Common.Transcendental
	, module MathPrelude.Common.Real
	) where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Logic
import MathPrelude.Structures.NumEq

import MathPrelude.Structures.Abelian
import MathPrelude.Structures.Ring
import MathPrelude.Structures.EuclideanDomain
import MathPrelude.Structures.Field
import MathPrelude.Structures.Module

import MathPrelude.Structures.Polynomial
import MathPrelude.Structures.PowerSeries
import MathPrelude.Structures.Complex
import MathPrelude.Structures.Ratio
import MathPrelude.Structures.Quotient

import MathPrelude.Structures.Derivation
import MathPrelude.Structures.Norm

import MathPrelude.Common.PreludeNumConst
import MathPrelude.Common.Integral
import MathPrelude.Common.CharZero
import MathPrelude.Common.Transcendental
import MathPrelude.Common.Real

import MathPrelude.Extras.Combinatorics
import MathPrelude.Extras.EllipticIntegrals
import MathPrelude.Extras.Evaluable
import MathPrelude.Extras.Integration
import MathPrelude.Extras.NewtonsMethod
import MathPrelude.Extras.NumberRings
import MathPrelude.Extras.Path
import MathPrelude.Extras.PolynomialFactorisation
import MathPrelude.Extras.Projective
import MathPrelude.Extras.Sequence
