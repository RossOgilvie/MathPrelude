{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | A derivation is generalisation of taking the derivative of a function. In most cases however it will actually just be the derivative you expect
module MathPrelude.Calculus.Derivation where

import qualified Prelude as P
import MathPrelude

import           MathPrelude.Classes.Module
import           MathPrelude.Classes.Norm
import           MathPrelude.Constructions.Polynomial
import           MathPrelude.Constructions.Complex

-- | A class representing derivable types. A derivation is a map such that the 'constants' are mapped to zero, and Leibniz' rule holds ie d(ab) = da b + a db
class Ring r ⇒ Derivation r where
  derive ∷ r → r

-- | A class representing integrable types.
-- class Integration r where
--   integrate ∷ r → r

-----------------------------------
--- Instances
-----------------------------------
instance Derivation Integer where derive _ = zero;
instance Derivation Int where derive _ = zero;
instance Derivation Int32 where derive _ = zero;
instance Derivation Int64 where derive _ = zero;
instance Derivation Float where derive _ = zero;
instance Derivation Double where derive _ = zero;



-----------------------------------
--- Data
-----------------------------------

-- | A Diff represents a value and possibly the values of the derivatives. A function valued in Diff therefore contains the information of its derivative.
data Diff a = D !a (Diff a) | C !a

-- Possibly it would be neater to implement Diff with a single constructor, iso to an infinite list of a the value and the values of the derivative. I have a feeling that this way, explictly tracking the constants, is faster though (if the number of derivatives is not insane) as we can be strict.
-- Another modification could be to implement the terminal construction as Z for zero. A constant x = D x Z (ie has zero derivative). Some functions are not defined at zero though.

-----------------------------------
--- Methods
-----------------------------------

-- | Lift a regular value to Diff as a constant.
constant ∷ a → Diff a
constant = C

-- | The identity function, f(x) = x. Compose with this function to create functions valued in Diff.
variable ∷ Ring a ⇒ a → Diff a
variable x = D x 1

-- | Extract the value of a Diff.
value ∷ Diff a → a
value (D x _) = x
value (C x) = x

-- | Take a derivative.
deriv ∷ Ring a ⇒ Diff a → Diff a
deriv (D _ x') = x'
deriv (C _) = C 0

-----------------------------------
--- Instances
-----------------------------------

instance Show a ⇒ Show (Diff a) where
  show = show' . value

instance Ring a ⇒ Derivation (Diff a) where
  derive = deriv

-- | How to differentiate a function valued in Diff.
instance Ring b ⇒ Derivation (a → Diff b) where
  derive f x = derive (f x)

instance Approx a ⇒ Approx (Diff a) where
  (=~) (D x x') (D y y') = x =~ y && x' =~ y'
  (=~) (C x) (C y) = x =~ y
  (=~) _ _ = False

instance Monoid a ⇒ Monoid (Diff a) where
  mempty = C mempty
  mappend (D x x') (D y y') = D (x<>y) (x'<>y')
  mappend (D x x') (C y) = D (x<>y) x'
  mappend (C x) (D y y') = D (x<>y) y'
  mappend (C x) (C y) = C (x<>y)

instance Group a ⇒ Group (Diff a) where
  negate (D x x') = D (negate x) (negate x')
  negate (C x) = C (negate x)

instance Abelian a ⇒ Abelian (Diff a) where

instance Ring r ⇒ Module (Diff r) r where
  scale r (D x x') = D (r*x) (scale r x')
  scale r (C x) = C (r*x)

instance Ring a ⇒ Ring (Diff a) where
  one = C one
  (*) (D x x') (D y y') = D (x*y) (x.*y'+y.*x')
  (*) (D x x') (C y) = D (x*y) (y.*x')
  (*) (C x) (D y y') = D (x*y) (x.*y')
  (*) (C x) (C y) = C (x*y)
  fromInteger = C . fromInteger

instance CRing a ⇒ CRing (Diff a)

instance IntDom a ⇒ IntDom (Diff a)

instance Field a ⇒ Field (Diff a) where
  recip (D x x') = D (recip x) (negate x' /. (x*x))
  recip (C x) = C (recip x)

instance (Field a, Transcendental a) ⇒ Transcendental (Diff a) where
  pi = C pi
  exp (D x x') = D (exp x) (exp x .* x')
  exp (C x) = C (exp x)
  log (D x x') = D (log x) (x' /. log x)
  log (C x) = C (log x)
  --(**) -- use default
  sqrt (D x x') = D (sqrt x) (x' /. (2*sqrt x))
  sqrt (C x) = C (sqrt x)
  --logBase -- use default
  sin (D x x') = D (sin x) (cos x .* x')
  sin (C x) = C (sin x)
  cos (D x x') = D (cos x) (- sin x .* x')
  cos (C x) = C (cos x)
  --tan  -- use default
  asin (D x x') = D (asin x) (x' /. sqrt (1 - x*x))
  asin (C x) = C (asin x)
  acos (D x x') = D (acos x) (-x' /. sqrt (1 - x*x))
  acos (C x) = C (acos x)
  atan (D x x') = D (atan x) (x' /. (1 + x*x))
  atan (C x) = C (acos x)
  atan2 (D y y') (D x x') = D (atan2 y x) ((x.*y'-y.*x')/.(x*x+y*y))
  atan2 (D y y') (C x) = D (atan2 y x) ((x.*y')/.(x*x+y*y))
  atan2 (C y) (D x x') = D (atan2 y x) ((-y.*x')/.(x*x+y*y))
  atan2 (C y) (C x) = C (atan2 y x)
  sinh (D x x') = D (cosh x) (cosh x .* x')
  sinh (C x) = C (cosh x)
  cosh (D x x') = D (sinh x) (sinh x .* x')
  cosh (C x) = C (sinh x)
  --tanh  -- use default
  --ahyps -- use default

instance CharZero a ⇒ CharZero (Diff a) where fromRational' = C . fromRational'
instance Q a ⇒ Q (Diff a) where toRational = toRational . value
instance Fractional a ⇒ Fractional (Diff a)

instance (Group v, Ord v, Norm v s, Ring s) ⇒ Norm (Diff v) (Diff s) where
    norm (D x _) = D (norm x) y'
        where
            y'
                | x > zero = 1
                | x < zero = -1
                | otherwise = 0
    norm (C x) = C (norm x)

instance (Ring a, Derivation a) ⇒ Derivation (Poly a) where
    -- differentiate the coefficients and then the variable
	derive p = map derive p + termwiseP polyDiff p
		where
			polyDiff n a
				| n /= 0 = (n-1, n' * a)
				| otherwise = (0, 0)
				where n' = fromInteger n

instance Derivation a ⇒ Derivation (Complex a) where
    derive = map derive
