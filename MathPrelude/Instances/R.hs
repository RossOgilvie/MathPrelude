{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Instances.R where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Field
import MathPrelude.Instances.Z
-- import MathPrelude.Representations.PreludeNumConst

class R a where
	fromDouble :: Double -> a
	toDouble :: a -> Double
	convR :: a -> a

	convR = fromDouble . toDouble

class R a => Real a where

class R a => RealFrac a where

class R a => RealFloat a where
	floatRadix          :: a -> Integer
	floatDigits         :: a -> Int
	floatRange          :: a -> (Int,Int)
	decodeFloat         :: a -> (Integer,Int)
	encodeFloat         :: Integer -> Int -> a
	exponent            :: a -> Int
	significand         :: a -> a
	scaleFloat          :: Int -> a -> a
	isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE :: a -> Bool
