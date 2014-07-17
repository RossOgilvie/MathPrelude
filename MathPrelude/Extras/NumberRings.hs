{-# LANGUAGE RebindableSyntax, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module MathPrelude.Extras.NumberRings
  ( module MathPrelude.Constructions.Quotient
  , Z2(..), z2
  , Z3(..), z3
  , Z4(..), z4
  , Z5(..), z5
  , Z6(..), z6
  , Z7(..), z7
  , Z8(..), z8
  , Z9(..), z9
  , Z10(..), z10
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

findRecip m z = fst $ extEuclidAlg z m

newtype Z2 = Z2 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Abelian
  , Ring
  )
z2 z = Z2 $ proj 2 z
instance IntDom Z2
instance Field Z2 where recip (Z2 z) = z2 $ findRecip 2 (element z)

newtype Z3 = Z3 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Abelian
  , Ring
  )
z3 z = Z3 $ proj 3 z
instance IntDom Z3
instance Field Z3 where recip (Z3 z) = z3 $ findRecip 3 (element z)

newtype Z4 = Z4 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Abelian
  , Ring
  )
z4 z = Z4 $ proj 4 z

newtype Z5 = Z5 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Abelian
  , Ring
  )
z5 z = Z5 $ proj 5 z
instance IntDom Z5
instance Field Z5 where recip (Z5 z) = z5 $ findRecip 5 (element z)

newtype Z6 = Z6 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Abelian
  , Ring
  )
z6 z = Z6 $ proj 6 z

newtype Z7 = Z7 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Abelian
  , Ring
  )
z7 z = Z7 $ proj 7 z
instance IntDom Z7
instance Field Z7 where recip (Z7 z) = z7 $ findRecip 7 (element z)

newtype Z8 = Z8 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Abelian
  , Ring
  )
z8 z = Z8 $ proj 8 z

newtype Z9 = Z9 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Abelian
  , Ring
  )
z9 z = Z9 $ proj 9 z

newtype Z10 = Z10 (Quotient Integer) deriving
  ( Show, NumEq
  , Monoid
  , Abelian
  , Ring
  )
z10 z = Z10 $ proj 10 z

newtype Qr2 = Qr2 (Quotient (Poly Rational)) deriving
  ( Show, NumEq
  , Monoid
  , Abelian
  , Ring
  )
r2poly :: Poly Rational
r2poly = poly [-2,0,1]
qr2 q1 q2 = Qr2 $ proj r2poly (poly [q1, q2])
instance IntDom Qr2
instance Field Qr2 where recip (Qr2 z) = Qr2 . proj r2poly . findRecip r2poly $ element z

type QuadExt = Quotient (Poly Rational)
quadRoot :: Rational -> Poly Rational
quadRoot a = poly [-a,0,1]
quadExt a q1 q2 = proj (quadRoot a) (poly [q1, q2])
