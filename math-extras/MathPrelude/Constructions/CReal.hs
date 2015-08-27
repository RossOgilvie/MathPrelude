{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}

-- | The computable reals, to arbitrary precision. Wraps Data.Number.CReal
module MathPrelude.Constructions.CReal
  ( phi
  , ramanujan
  ) where

import MathPrelude
import qualified Prelude                            as P

import           Data.Number.CReal

import           MathPrelude.Classes.Field
import           MathPrelude.Classes.Integral
import           MathPrelude.Classes.Rational
import           MathPrelude.Classes.Real
import           MathPrelude.Classes.Transcendental

instance NumEq CReal where
  (=~) = (==)

instance Monoid CReal where
  mempty = 0
  mappend = (P.+)

instance Group CReal where
  negate = P.negate
  (-) = (P.-)

instance Abelian CReal

instance Ring CReal where
  one = 1
  (*) = (P.*)
  fromInteger = P.fromInteger

instance CRing CReal

instance IntDom CReal

instance Field CReal where
  recip = P.recip

instance CharZero CReal where
  fromRational' = P.fromRational . toRational98

instance Transcendental CReal where
  pi = P.pi
  exp = P.exp
  sqrt = P.sqrt
  log = P.log
  (**) = (P.**)
  logBase = P.logBase
  sin = P.sin
  cos = P.cos
  tan = P.tan
  asin = P.asin
  acos = P.acos
  atan = P.atan
  atan2 = P.atan2
  sinh = P.sinh
  cosh = P.cosh
  tanh = P.tanh
  asinh = P.asinh
  acosh = P.acosh
  atanh = P.atanh

instance Real CReal where
  fromDouble = P.fromRational . toRational98 .toRational
  toDouble = P.read . showCReal 20
  truncate = fromIntegral98 . P.truncate
  round = fromIntegral98 . P.round
  ceiling = fromIntegral98 . P.ceiling
  floor = fromIntegral98 . P.floor

instance RealFrac CReal
instance RealFloat CReal where
  floatRadix = P.floatRadix
  floatDigits = P.floatDigits
  floatRange = P.floatRange
  decodeFloat = P.decodeFloat
  encodeFloat = P.encodeFloat
  exponent = P.exponent
  significand = P.significand
  scaleFloat = P.scaleFloat
  isNaN = P.isNaN
  isInfinite = P.isInfinite
  isDenormalized = P.isDenormalized
  isNegativeZero = P.isNegativeZero
  isIEEE = P.isIEEE

-- | The golden ratio
phi ∷ CReal
phi = (1 + P.sqrt 5)/2

-- | Ramanujan's constant. It is almost an integer, despite being e^(pi * sqrt 163)
ramanujan :: CReal
ramanujan = exp (pi * sqrt 163)
