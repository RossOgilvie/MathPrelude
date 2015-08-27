{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE UnicodeSyntax         #-}

-- | An attempt at making a generic projectivisation operation, ie the operation of adding an infinity. It makes the arithmetic kinda screw-y though. This works well enough for computing cross rations, and even better and Mobius transformations.
module MathPrelude.Constructions.Projective
  ( Proj
  , fromField
  , crossRatio
  , MobiusT(..)
  ) where

-----------------------------------
--- Imports
-----------------------------------

import MathPrelude
-- import           MathPrelude.Classes.Action
-- import           MathPrelude.Classes.Derivation
import           MathPrelude.Classes.Field
import           MathPrelude.Classes.Rational
import           MathPrelude.Constructions.Complex
import qualified Prelude                           as P

-----------------------------------
--- Proj
-----------------------------------
-- | A data type representing the projectivisation of a given type, ie the type with a "point at infinity added". First order infinitessimal information is stored at zero and infinity.
data Proj a = Elem a | Zero a | Infty a

-- | Complex projective space, aka CP^1
type CP = Proj (Complex Double)


-----------------------------------
--- Methods
-----------------------------------
-- | Inject a field into its projectivisation.
fromField ∷ Field a ⇒ a → Proj a
fromField x
  | x =~ 0 = Zero (x/epsilon)
  | recip x =~ 0 = Infty (x*epsilon)
  | otherwise = Elem x

-- | Calculate a cross-ratio.
crossRatio ∷ Field a ⇒ Proj a → Proj a → Proj a → Proj a → Proj a
crossRatio x y z w = (x-z)/(y-z)*(x-w)/(y-w)

-----------------------------------
--- Mobius Transformation
-----------------------------------
-- | Represents a Mobius transformation (aka fractional linear transformation) on a projective space.
data MobiusT a = MobiusT a a a a deriving Show

-----------------------------------
--- Instances -- Proj
-----------------------------------

instance Show a ⇒ Show (Proj a) where
  show (Zero _) = "0"
  show (Elem a) = P.show a
  show (Infty _) = "Infinity"

instance Eq a ⇒ Eq (Proj a) where
  (==) (Zero _) (Zero _) = True
  (==) (Elem a) (Elem b) = a==b
  (==) (Infty _) (Infty _) = True
  (==) _ _ = False

instance NumEq a ⇒ NumEq (Proj a) where
  (=~) (Zero _) (Zero _) = True
  (=~) (Elem a) (Elem b) = a=~b
  (=~) (Infty _) (Infty _) = True
  -- epsilon = Elem epsilon
  -- nearZero = (>>~) (Zero epsilon)
  -- (>>~) (Zero _) (Zero _) = True
  -- (>>~) (Zero _) (Elem a) = nearZero a
  -- (>>~) (Zero _) (Infty _) = False
  -- (>>~) (Elem _) (Zero _) = True
  -- (>>~) (Elem a) (Elem b) = a >>~ b
  -- (>>~) (Elem _) (Infty _) = False
  -- (>>~) (Infty _) _ = True

instance Ord a ⇒ Ord (Proj a) where
  compare (Zero _) (Zero _) = EQ
  compare (Zero _) (Elem _) = LT
  compare (Zero _) (Infty _) = LT
  compare (Elem _) (Zero _) = GT
  compare (Elem a) (Elem b) = compare a b
  compare (Elem _) (Infty _) = LT
  compare (Infty _) (Zero _) = GT
  compare (Infty _) (Elem _) = GT
  compare (Infty _) (Infty _) = EQ

-- instance (Field a, Derivation a) ⇒ Derivation (Proj a) where
--   derive (Zero a) = Zero (derive a)
--   derive (Elem a) = Elem (derive a)
--   derive (Infty a) = Infty (derive a)


instance Ring a ⇒ Monoid (Proj a) where
  mempty = Zero one
  mappend (Zero a) (Zero b) = Zero (a+b)
  mappend (Elem a) (Elem b) = Elem (a+b)
  mappend (Infty a) (Infty b) = Infty (a+b)
  mappend (Zero _) (Elem b) = Elem b
  mappend (Elem b) (Zero _) = Elem b
  mappend (Zero _) (Infty b) = Infty b
  mappend (Infty b) (Zero _) = Infty b
  mappend (Elem _) (Infty b) = Infty b
  mappend (Infty b) (Elem _) = Infty b

instance Ring a ⇒ Group (Proj a) where
  negate (Zero a) = Zero (negate a)
  negate (Elem a) = Elem (negate a)
  negate (Infty a) = Infty (negate a)
instance Ring a ⇒ Abelian (Proj a)

instance Field a ⇒ Ring (Proj a) where
  one = Elem one
  (*) (Zero a) (Zero b) = Zero (a*b*epsilon)
  (*) (Elem a) (Elem b) = Elem (a*b)
  (*) (Infty a) (Infty b) = Infty (a*b/epsilon)
  (*) (Zero a) (Elem b) = Zero (a*b)
  (*) (Elem a) (Zero b) = Zero (a*b)
  (*) (Zero a) (Infty b) = Elem (a*b)
  (*) (Infty a) (Zero b) = Elem (a*b)
  (*) (Elem a) (Infty b) = Infty (a*b)
  (*) (Infty a) (Elem b) = Infty (a*b)

instance Field a ⇒ CRing (Proj a)
instance Field a ⇒ IntDom (Proj a)

instance Field a ⇒ Field (Proj a) where
  recip (Zero a) = Infty (recip a)
  recip (Elem a) = Elem (recip a)
  recip (Infty a) = Zero (recip a)

instance (Eq a, CharZero a, Field a) ⇒ CharZero (Proj a) where
  fromRational' = fromField . fromRational'


-----------------------------------
--- Instances -- MobiusT
-----------------------------------
instance (Eq a, Field a) ⇒ Eq (MobiusT a) where
  (==) (MobiusT a b c d) (MobiusT x y z w) = r1==r2 && r2 == r3 && r3 == r4
    where
      r1 = a/x
      r2 = b/y
      r3 = c/z
      r4 = d/w

instance Field a ⇒ NumEq (MobiusT a) where
  (=~) (MobiusT a b c d) (MobiusT x y z w) = r1=~r2 && r2 =~ r3 && r3 =~ r4
    where
      r1 = a/x
      r2 = b/y
      r3 = c/z
      r4 = d/w


-- instance Field a ⇒ Action (MobiusT a) a a where
--   act (MobiusT a b c d) = \z → (a*z + b)/(c*z + d)
