{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
module MathPrelude.Classes.Norm where

import BasicPrelude
import qualified Prelude as P

-- import MathPrelude.Common.PreludeNumConst
import MathPrelude.Common.Integral
import MathPrelude.Common.Real

-- | A class for norms. BROKEN/NEEDS SERIOUS THOUGHT AND REVISION
class Norm a where
  norm ∷ Real b ⇒ a → b


instance Norm Int where
  norm = fromDouble . fromIntegral . P.abs
instance Norm Int32 where
  norm = fromDouble . fromIntegral . P.abs
instance Norm Int64 where
  norm = fromDouble . fromIntegral . P.abs
instance Norm Integer where
  norm = fromDouble . fromIntegral . P.abs

instance Norm Float where
  norm = convReal . P.abs
instance Norm Double where
  norm = fromDouble . P.abs
