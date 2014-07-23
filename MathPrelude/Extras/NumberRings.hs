{-# LANGUAGE RebindableSyntax, UnicodeSyntax, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module MathPrelude.Extras.NumberRings
  ( module MathPrelude.Constructions.Quotient
  -- * Rings of the form Z/nZ
  , Z2(..), z2
  , Z3(..), z3
  , Z4(..), z4
  , Z5(..), z5
  , Z6(..), z6
  , Z7(..), z7
  , Z8(..), z8
  , Z9(..), z9
  , Z10(..), z10
  -- * Quadratic extentions of the rationals
  , Qr2(..), qr2
  , QuadExt(..), quadExt
  )  where

import BasicPrelude
import MathPrelude.Constructions.Quotient
import MathPrelude.Constructions.Polynomial
import MathPrelude.Common.Rational
-- import MathPrelude.Constructions.Complex
-- import MathPrelude.Algebraic.Module
-- import MathPrelude.Common.Transcendental

-- | Find a reciprocal in a finite field using the extended Euclidean algorithm.
findRecip m z = fst $ extEuclidAlg z m

-- | The ring Z/2Z.
newtype Z2 = Z2 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Group
	, Abelian
  , Ring
  )
-- | Project to the ring Z/2Z.
z2 z = Z2 $ proj 2 z
instance IntDom Z2
instance Field Z2 where recip (Z2 z) = z2 $ findRecip 2 (element z)

-- | The ring Z/3Z.
newtype Z3 = Z3 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Group
	, Abelian
  , Ring
  )
-- | Project to the ring Z/3Z.
z3 z = Z3 $ proj 3 z
instance IntDom Z3
instance Field Z3 where recip (Z3 z) = z3 $ findRecip 3 (element z)

-- | The ring Z/4Z.
newtype Z4 = Z4 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Group
	, Abelian
  , Ring
  )
-- | Project to the ring Z/4Z.
z4 z = Z4 $ proj 4 z

-- | The ring Z/5Z.
newtype Z5 = Z5 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Group
	, Abelian
  , Ring
  )
-- | Project to the ring Z/5Z.
z5 z = Z5 $ proj 5 z
instance IntDom Z5
instance Field Z5 where recip (Z5 z) = z5 $ findRecip 5 (element z)

-- | The ring Z/6Z.
newtype Z6 = Z6 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Group
	, Abelian
  , Ring
  )
z6 z = Z6 $ proj 6 z
-- | Project to the ring Z/6Z.

-- | The ring Z/7Z.
newtype Z7 = Z7 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Group
	, Abelian
  , Ring
  )
-- | Project to the ring Z/7Z.
z7 z = Z7 $ proj 7 z
instance IntDom Z7
instance Field Z7 where recip (Z7 z) = z7 $ findRecip 7 (element z)

-- | The ring Z/8Z.
newtype Z8 = Z8 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Group
	, Abelian
  , Ring
  )
-- | Project to the ring Z/8Z.
z8 z = Z8 $ proj 8 z

-- | The ring Z/9Z.
newtype Z9 = Z9 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Group
	, Abelian
  , Ring
  )
-- | Project to the ring Z/9Z.
z9 z = Z9 $ proj 9 z

-- | The ring Z/10Z.
newtype Z10 = Z10 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Group
	, Abelian
  , Ring
  )
-- | Project to the ring Z/10Z.
z10 z = Z10 $ proj 10 z

-- | The quadratic extenion of the rationals by the sqrt of 2.
newtype Qr2 = Qr2 (Quotient (Poly Rational)) deriving
  ( Show, NumEq
  , Monoid
  , Group
	, Abelian
  , Ring
  )
-- | Project to the quadratic extenion of the rationals by the sqrt of 2.
r2poly ∷ Poly Rational
r2poly = poly [-2,0,1]
-- | Construct the element qr2 q1 q2 = q_1 + q2*sqrt(2)
qr2 q1 q2 = Qr2 $ proj r2poly (poly [q1, q2])
instance IntDom Qr2
instance Field Qr2 where recip (Qr2 z) = Qr2 . proj r2poly . findRecip r2poly $ element z

-- | An arbitrary quadratic extension of the rationals.
type QuadExt = Quotient (Poly Rational)
-- | A poly with roots of the sqrt of the given rational.
quadRoot ∷ Rational → Poly Rational
quadRoot a = poly [-a,0,1]
-- | Construct the element quadExt a q1 q2 = q_1 + q2*sqrt(a)
quadExt a q1 q2 = proj (quadRoot a) (poly [q1, q2])
