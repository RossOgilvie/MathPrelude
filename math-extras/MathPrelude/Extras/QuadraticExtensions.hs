{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE UnicodeSyntax              #-}
module MathPrelude.Extras.QuadraticExtensions
  (
  -- * Quadratic extentions of the rationals
  Qr2(..), qr2
  , QuadExt(..), quadExt
  )  where

import MathPrelude
import           MathPrelude.Classes.EuclideanDomain
import           MathPrelude.Classes.Field
import           MathPrelude.Classes.Rational
import           MathPrelude.Constructions.Polynomial
import           MathPrelude.Constructions.Quotient

-- | The quadratic extenion of the rationals by the sqrt of 2.
newtype Qr2 = Qr2 (Quotient (Poly Rational)) deriving
  ( Show
  , NumEq
  , Monoid
  , Group
  , Abelian
  , Ring
  , CRing
  )
-- | Project to the quadratic extenion of the rationals by the sqrt of 2.
r2poly ∷ Poly Rational
r2poly = fromListP [-2,0,1]
-- | Construct the element qr2 q1 q2 = q_1 + q2*sqrt(2)
qr2 q1 q2 = Qr2 $ proj r2poly (fromListP [q1, q2])
instance IntDom Qr2
instance Field Qr2 where recip (Qr2 z) = Qr2 . proj r2poly . findRecip r2poly $ element z

-- | An arbitrary quadratic extension of the rationals.
type QuadExt = Quotient (Poly Rational)
-- | A poly with roots of the sqrt of the given rational.
quadRoot ∷ Rational → Poly Rational
quadRoot a = fromListP [-a,0,1]
-- | Construct the element quadExt a q1 q2 = q_1 + q2*sqrt(a)
quadExt a q1 q2 = proj (quadRoot a) (fromListP [q1, q2])

findRecip m z = fst $ extEuclidAlg z m
