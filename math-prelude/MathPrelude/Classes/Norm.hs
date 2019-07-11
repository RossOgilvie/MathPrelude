{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE UnicodeSyntax          #-}

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | A Norm is a generalisation of the notion of length or magnituge of a vector. The archetypal norm is just the length of a vector, though the absolute value of a number is more trivial example. Should be prefered over 'abs'. Also contains classes for inner products, aka dot product.
module MathPrelude.Classes.Norm
  ( Norm(..)
  , InnerProd(..)
  , ComplexInnerProd(..)
  )
where

import           MathPrelude.Prelude.CorePrelude
import           MathPrelude.Prelude.NamedNumbers
import qualified Prelude                       as P

-- | The norm is the length of a object. If the object is a vector, the norm should commute with positive scaling and obey the triangle inequality.
class Norm v s | v → s where
  norm ∷ v → s

-- | An inner product is a billinear operation, that is positive definite on the diagonal (iprod v v >= 0, with equality iff v ==0) and nondegenerate.
class InnerProd v s | v → s where
  iprod ∷ v → v → s

-- | A complex inner product is much the same as an inner product, but is insead sequilinear, ie only conjugate linear in the second argument.
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
