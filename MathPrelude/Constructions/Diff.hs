{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE UnicodeSyntax         #-}
module MathPrelude.Constructions.Diff
  ( module MathPrelude.Classes.Derivation
  , Diff()
  , constant
  , variable
  , value
  ) where

-- borrowing heavily from https://hackage.haskell.org/package/numbers-3000.2.0.1/docs/Data-Number-Dif.html

-----------------------------------
--- Imports
-----------------------------------
import           BasicPrelude
import qualified Prelude                            as P

import           MathPrelude.Classes.Derivation
import           MathPrelude.Classes.Field
import           MathPrelude.Classes.Module
import           MathPrelude.Classes.Transcendental

data Diff a = D !a (Diff a) | C !a

constant ∷ a → Diff a
constant = C

variable ∷ Ring a ⇒ a → Diff a
variable x = D x 1

value ∷ Diff a → a
value (D x _) = x
value (C x) = x

deriv ∷ Ring a ⇒ Diff a → Diff a
deriv (D _ x') = x'
deriv (C _) = C 0

instance Show a ⇒ Show (Diff a) where
  show = P.show . value

instance Ring a ⇒ Derivation (Diff a) where
  derive = deriv

instance Ring b ⇒ Derivation (a → Diff b) where
  derive f x = derive (f x)

instance NumEq a ⇒ NumEq (Diff a) where
  (=~) (D x x') (D y y') = x =~ y && x' =~ y'
  (=~) (C x) (C y) = x =~ y
  (=~) _ _ = False

instance Monoid a ⇒ Monoid (Diff a) where
  mempty = C mempty
  mappend (D x x') (D y y') = D (x<>y) (x'<>y')
  mappend (D x x') (C y) = D (x<>y) (x')
  mappend (C x) (D y y') = D (x<>y) (y')
  mappend (C x) (C y) = C (x<>y)

instance Group a ⇒ Group (Diff a) where
  negate (D x x') = D (negate x) (negate x')
  negate (C x) = C (negate x)
instance Abelian a ⇒ Abelian (Diff a) where
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
-- instance Module m r ⇒ Module (Diff m) r where
instance Ring r ⇒ Module (Diff r) r where
  scale r (D x x') = D (r*x) (scale r x')
  scale r (C x) = C (r*x)


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
  atan2 x y
    | nearZero x = zero
    | otherwise = atan (y/x)
  sinh (D x x') = D (cosh x) (cosh x .* x')
  sinh (C x) = C (cosh x)
  cosh (D x x') = D (sinh x) (sinh x .* x')
  cosh (C x) = C (sinh x)
  --tanh  -- use default
  --ahyps -- use defaults
