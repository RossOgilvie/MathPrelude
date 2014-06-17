{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Common.Integral
	( Integral(..)
	, fromIntegral98
	, toIntegral98
	) where

-----------------------------------
--- Imports
-----------------------------------
import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Ring

-----------------------------------
--- Classes
-----------------------------------
class Ring a => Integral a where
	toInteger :: a -> Integer
	convInteger :: Integral b => a -> b

	convInteger =  fromInteger . toInteger

-----------------------------------
--- Methods
-----------------------------------
fromIntegral98 :: (P.Integral a, Integral b) => a -> b
fromIntegral98 = fromInteger . P.fromIntegral
toIntegral98 :: (Integral a, P.Integral b) => a -> b
toIntegral98 = P.fromIntegral . toInteger

-----------------------------------
--- Instances
-----------------------------------
instance Integral Integer where toInteger = id
instance Integral Int where toInteger = P.toInteger
instance Integral Int32 where toInteger = P.toInteger
instance Integral Int64 where toInteger = P.toInteger
