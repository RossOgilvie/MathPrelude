{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
module MathPrelude.Classes.Norm
  ( Norm(..)
  , InnerProd(..)
  , ComplexInnerProd(..)
  ) where

import BasicPrelude
import qualified Prelude as P

-- import MathPrelude.Classes.Module
import MathPrelude.Classes.Integral
import MathPrelude.Classes.Transcendental

-- | A class for norms.
class Norm v s | v → s where
  norm ∷ v → s

class InnerProd v s | v → s where
  iprod ∷ v → v → s

class ComplexInnerProd v s | v → s where
  cxiprod ∷ v → v → s

-- instance (Transcendental s, InnerProd v s) ⇒ Norm v s where
--   norm v = sqrt $ iprod v v

instance Norm Int Int where
  norm = P.abs
instance Norm Int32 Int32 where
  norm = P.abs
instance Norm Int64 Int64 where
  norm = P.abs
instance Norm Integer Integer where
  norm = P.abs

instance Norm Float Float where
  norm = P.abs
instance Norm Double Double where
  norm = P.abs
