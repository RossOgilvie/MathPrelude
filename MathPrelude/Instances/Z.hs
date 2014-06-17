{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Instances.Z where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.EuclideanDomain
import MathPrelude.Representations.PreludeNumConst

class Z a where
	fromInteger :: Integer -> a
	toInteger :: a -> Integer
	convZ :: a -> a

	convZ =  fromInteger . toInteger

-- Compatibility
class Z a => Integral a
