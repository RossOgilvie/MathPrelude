{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Common.Real where

------------------------------
--- Imports
------------------------------
import BasicPrelude
import qualified Prelude as P


import qualified GHC.Float as GF

import MathPrelude.Structures.Ring
import MathPrelude.Common.Integral

------------------------------
--- Classes
------------------------------

class Real a where
	fromDouble :: Double -> a
	toDouble :: a -> Double
	convReal :: Real b => a -> b

	truncate, round, ceiling, floor :: Integral b => a -> b

	convReal = fromDouble . toDouble
	truncate = floor
	floor x = ceiling x - one
	ceiling x = floor x + one


class Real a => RealFrac a

class Real a => RealFloat a where
	floatRadix          :: a -> Integer
	floatDigits         :: a -> Int
	floatRange          :: a -> (Int,Int)
	decodeFloat         :: a -> (Integer,Int)
	encodeFloat         :: Integer -> Int -> a
	exponent            :: a -> Int
	significand         :: a -> a
	scaleFloat          :: Int -> a -> a
	isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE :: a -> Bool

------------------------------
--- Methods
------------------------------

------------------------------
--- Instances
------------------------------

instance Real Double where
	fromDouble = id
	toDouble = id
	truncate = fromIntegral98 . P.truncate
	round = fromIntegral98 . P.round
	ceiling = fromIntegral98 . P.ceiling
	floor = fromIntegral98 . P.floor

instance Real Float where
	fromDouble = GF.double2Float
	toDouble = GF.float2Double
	truncate = fromIntegral98 . P.truncate
	round = fromIntegral98 . P.round
	ceiling = fromIntegral98 . P.ceiling
	floor = fromIntegral98 . P.floor


instance RealFrac Float
instance RealFrac Double


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
