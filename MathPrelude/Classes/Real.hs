-- | A module for converting between types that represent real numbers, and rounding operators.
module MathPrelude.Classes.Real where

------------------------------
--- Imports
------------------------------
import           MathPrelude.Prelude.CorePrelude
import qualified Prelude                      as P

import qualified GHC.Float                    as GF

import           MathPrelude.Classes.Field
import           MathPrelude.Classes.Integral

------------------------------
--- Classes
------------------------------
-- | A class representing real numbers. The canonical representation is sadly the 'Double'.
class (Field a, Ord a) ⇒ Real a where
    -- | Marshall a Double
    fromDouble ∷ Double → a
    -- | Export a Double
    toDouble ∷ a → Double
    -- | Convert between any two real types.
    convReal ∷ Real b ⇒ a → b

    -- Recover an integral type by rounding.
    truncate, round, ceiling, floor ∷ Integral b ⇒ a → b

    convReal = fromDouble . toDouble
    truncate = floor
    floor x = ceiling x - one
    ceiling x = truncate x + one

    {-# MINIMAL fromDouble, toDouble, round, (floor | ceiling | truncate) #-}

-- | A compatibility class.
class Real a ⇒ RealFrac a

-- | A class containing all the operations that examine parts of floating point numbers. Same as the prelude's version, but without atan2.
class Real a ⇒ RealFloat a where
    floatRadix          ∷ a → Integer
    floatDigits         ∷ a → Int
    floatRange          ∷ a → (Int,Int)
    decodeFloat         ∷ a → (Integer,Int)
    encodeFloat         ∷ Integer → Int → a
    exponent            ∷ a → Int
    significand         ∷ a → a
    scaleFloat          ∷ Int → a → a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE ∷ a → Bool

------------------------------
--- Methods
------------------------------

------------------------------
--- Instances
------------------------------
-- | Real instance for Double
instance Real Double where
    fromDouble = id
    toDouble = id
    truncate = fromIntegral98 . P.truncate
    round = fromIntegral98 . P.round
    ceiling = fromIntegral98 . P.ceiling
    floor = fromIntegral98 . P.floor

-- | Real instance for Float
instance Real Float where
    fromDouble = GF.double2Float
    toDouble = GF.float2Double
    truncate = fromIntegral98 . P.truncate
    round = fromIntegral98 . P.round
    ceiling = fromIntegral98 . P.ceiling
    floor = fromIntegral98 . P.floor


-- | RealFrac instance for Float
instance RealFrac Float
-- | RealFrac instance for Double
instance RealFrac Double


-- | RealFloat instance for Float
instance RealFloat Float where
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

-- | RealFloat instance for Double
instance RealFloat Double where
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
