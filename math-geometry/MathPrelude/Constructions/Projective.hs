{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE UnicodeSyntax         #-}

-- | An attempt at making a generic projectivisation operation, ie the operation of adding an infinity. It makes the arithmetic kinda screw-y though. This works well enough for computing cross rations, and even better at Mobius transformations.
module MathPrelude.Constructions.Projective
  ( Proj
  , CP1
  , fromField, fromFieldApprox
  , crossRatio
  , MobiusT(..)
  ) where

-----------------------------------
--- Imports
-----------------------------------

import MathPrelude
-- import           MathPrelude.Classes.Action
-- import           MathPrelude.Classes.Derivation
import           MathPrelude.Constructions.Complex
import qualified Prelude                           as P

-----------------------------------
--- Proj
-----------------------------------
-- | A data type representing the projectivisation of a given type, ie the type with a "point at infinity added".
data Proj a = Elem a | Zero | Infty

-- | Complex projective space, aka CP^1
type CP1 = Proj (Complex Double)


-----------------------------------
--- Methods
-----------------------------------
-- | Inject a field into its projectivisation.
fromField ∷ (Field a, Eq a) ⇒ a → Proj a
fromField x
  | x == 0 = Zero
  | otherwise = Elem x

fromFieldApprox ∷ (Field a, Approx a) ⇒ a → Proj a
fromFieldApprox x
  | nearZero x = Zero
  | nearZero (recip x) = Infty
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
  show Zero = "0"
  show (Elem a) = P.show a
  show Infty = "Infinity"

instance Eq a ⇒ Eq (Proj a) where
  (==) Zero Zero = True
  (==) (Elem a) (Elem b) = a==b
  (==) Infty Infty = True
  (==) _ _ = False

instance Approx a ⇒ Approx (Proj a) where
  (=~) Zero Zero = True
  (=~) (Elem a) (Elem b) = a=~b
  (=~) Infty Infty = True

instance Ord a ⇒ Ord (Proj a) where
  compare Zero Zero = EQ
  compare Zero (Elem _) = LT
  compare Zero Infty = LT
  compare (Elem _) Zero = GT
  compare (Elem a) (Elem b) = compare a b
  compare (Elem _) Infty = LT
  compare Infty Zero = GT
  compare Infty (Elem _) = GT
  compare Infty Infty = EQ

instance Ring a ⇒ Semigroup (Proj a) where
  (<>) Zero Zero = Zero
  (<>) (Elem a) (Elem b) = Elem (a+b)
  (<>) Infty Infty = Infty
  (<>) Zero (Elem b) = Elem b
  (<>) (Elem b) Zero = Elem b
  (<>) Zero Infty = Infty
  (<>) Infty Zero = Infty
  (<>) (Elem _) Infty = Infty
  (<>) Infty (Elem _) = Infty

instance Ring a ⇒ Monoid (Proj a) where
  mempty = Zero

instance Ring a ⇒ Group (Proj a) where
  negate Zero = Zero
  negate (Elem a) = Elem (negate a)
  negate Infty = Infty
instance Ring a ⇒ Abelian (Proj a)

instance Field a ⇒ Ring (Proj a) where
  one = Elem one
  (*) Zero Zero = Zero
  (*) (Elem a) (Elem b) = Elem (a*b)
  (*) Infty Infty = Infty
  (*) Zero (Elem b) = Zero
  (*) (Elem a) Zero = Zero
  (*) Zero Infty = Elem one
  (*) Infty Zero = Elem one
  (*) (Elem a) Infty = Infty
  (*) Infty (Elem b) = Infty

instance Field a ⇒ CRing (Proj a)
instance Field a ⇒ IntDom (Proj a)

instance Field a ⇒ Field (Proj a) where
  recip Zero = Infty
  recip (Elem a) = Elem (recip a)
  recip Infty = Zero

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

instance (Approx a, Field a) ⇒ Approx (MobiusT a) where
  (=~) (MobiusT a b c d) (MobiusT x y z w) = r1=~r2 && r2 =~ r3 && r3 =~ r4
    where
      r1 = a/x
      r2 = b/y
      r3 = c/z
      r4 = d/w


-- instance Field a ⇒ Action (MobiusT a) a a where
--   act (MobiusT a b c d) = \z → (a*z + b)/(c*z + d)
