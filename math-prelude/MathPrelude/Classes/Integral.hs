{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}

-- | A module for converting between integral types.
module MathPrelude.Classes.Integral
    ( Integral(..)
    , fromIntegral98
    , toIntegral98
    , fromIntegral
    -- * Misc
    , even
    , odd
    )
where

-----------------------------------
--- Imports
-----------------------------------
import           MathPrelude.Prelude.CorePrelude
import           MathPrelude.Prelude.NamedNumbers
import qualified Prelude                       as P

import           MathPrelude.Classes.Ring

-----------------------------------
--- Classes
-----------------------------------
-- | This class describes types that can be transformed into the canonical integer type Integer. All rings already have a map from Integer, and these should be inverses (at least partially).
class (Ring a, Enum a) ⇒ Integral a where
    -- | Convert to Integer
    toInteger ∷ a → Integer

-----------------------------------
--- Methods
-----------------------------------
-- | Convert from a Prelude defined Integral type to our Integral
fromIntegral98 :: (P.Integral a, Integral b) => a -> b
fromIntegral98 = fromInteger . P.fromIntegral
-- | Convert from our Integral type to the Prelude defined Integral type.
toIntegral98 :: (Integral a, P.Integral b) => a -> b
toIntegral98 = P.fromIntegral . toInteger

-- | Convert any integral to an element of any ring.
fromIntegral :: (Integral a, Ring b) => a -> b
fromIntegral = fromInteger . toInteger

-- | Test whether an integral is even.
even :: Integral a => a -> Bool
even = P.even . toInteger
-- | Test whether an integral is odd.
odd :: Integral a => a -> Bool
odd = P.odd . toInteger

-----------------------------------
--- Instances
-----------------------------------
-- instance P.Integral a ⇒ Integral a where
--     toInteger = P.toInteger
instance Integral Integer where
    toInteger = id
instance Integral Int where
    toInteger = P.toInteger
instance Integral Int32 where
    toInteger = P.toInteger
instance Integral Int64 where
    toInteger = P.toInteger
instance Integral Word where
    toInteger = P.toInteger
instance Integral Word32 where
    toInteger = P.toInteger
instance Integral Word64 where
    toInteger = P.toInteger

-----------------------------------
-- Rerwite Rules -- for great justice!
-----------------------------------

-- try not to inline these functions until the last phase (phases count down), so the rewrite rule can come into effect
{-# INLINE [0] fromIntegral98 #-}
{-# RULES "fromIntegral98/a→a" fromIntegral98 = id #-}

{-# INLINE [0] toIntegral98 #-}
{-# RULES "toIntegral98/a→a" toIntegral98 = id #-}

{-# INLINE [0] fromIntegral #-}
{-# RULES "fromIntegral/a→a" fromIntegral = id #-}
-- {-# RULES "fromIntegral/integerToInt" fromIntegral = GHC.integerToInt #-}
-- {-# RULES "fromIntegral/smallInteger" fromIntegral = GHC.smallInteger #-}
-- {-# RULES "fromIntegral/integerToInt64" fromIntegral = GHC.integerToInt64 #-}
-- {-# RULES "fromIntegral/int64ToInteger" fromIntegral = GHC.int64ToInteger #-}
