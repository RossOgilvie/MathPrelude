{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Common.Integral
	( Integral(..)
	, fromIntegral98
	, toIntegral98
	, fromIntegral
	, even
	, odd
	) where

-----------------------------------
--- Imports
-----------------------------------
import BasicPrelude
import qualified Prelude as P

import MathPrelude.Algebraic.Ring

-- import qualified GHC.Integer.Type as GHC

-----------------------------------
--- Classes
-----------------------------------
class (Ring a, Enum a) => Integral a where
	toInteger :: a -> Integer

-----------------------------------
--- Methods
-----------------------------------
fromIntegral98 :: (P.Integral a, Integral b) => a -> b
fromIntegral98 = fromInteger . P.fromIntegral
toIntegral98 :: (Integral a, P.Integral b) => a -> b
toIntegral98 = P.fromIntegral . toInteger

fromIntegral :: (Integral a, Ring b) => a -> b
fromIntegral =  fromInteger . toInteger

even :: Integral a => a -> Bool
even = P.even . toInteger
odd :: Integral a => a -> Bool
odd = P.odd . toInteger

-----------------------------------
--- Instances
-----------------------------------
instance Integral Integer where toInteger = id
instance Integral Int where toInteger = P.toInteger
instance Integral Int32 where toInteger = P.toInteger
instance Integral Int64 where toInteger = P.toInteger

-----------------------------------
-- Rerwite Rules -- for great justice!
-----------------------------------

-- try not to inline these functions until the last phase (phases count down), so the rewrite rule can come into effect
{-# INLINE [0] fromIntegral98 #-}
{-# RULES "fromIntegral98/a->a" fromIntegral98 = id #-}

{-# INLINE [0] toIntegral98 #-}
{-# RULES "toIntegral98/a->a" toIntegral98 = id #-}

{-# INLINE [0] fromIntegral #-}
{-# RULES "fromIntegral/a->a" fromIntegral = id #-}
-- {-# RULES "fromIntegral/integerToInt" fromIntegral = GHC.integerToInt #-}
-- {-# RULES "fromIntegral/smallInteger" fromIntegral = GHC.smallInteger #-}
-- {-# RULES "fromIntegral/integerToInt64" fromIntegral = GHC.integerToInt64 #-}
-- {-# RULES "fromIntegral/int64ToInteger" fromIntegral = GHC.int64ToInteger #-}
