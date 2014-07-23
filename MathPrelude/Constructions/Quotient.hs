{-# LANGUAGE RebindableSyntax, UnicodeSyntax, OverloadedStrings #-}
module MathPrelude.Constructions.Quotient
  ( module MathPrelude.Algebraic.EuclideanDomain
  , Quotient(..)
  , proj
  , liftQ, liftQ2, liftQ2'
  )  where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Algebraic.EuclideanDomain
import MathPrelude.Classes.Derivation


------------------------------
--- Quotient
------------------------------
-- | A representation of a ring quotiented by an ideal generated by the element "modulus". Can be quite un-type-safe, and throws errors when operations attempt to work with value from different quotients (different moduli). A modulus of Nothing represents an element of the ring, which will be projected to the appropriate constant when used.
data Quotient a = Quotient { modulus ∷ Maybe a, element ∷ a}

------------------------------
---- Methods
------------------------------
-- | The canonical projection for a ring to the quotient.
proj ∷ EuclideanDomain a ⇒ a → a → Quotient a
proj m x = Quotient {modulus = Just m, element = x `mod` m}

-- | The associated function on the quotient. Definitely not guaranteed to be well defined (ie may depend on the choice of coset representative).
liftQ ∷ EuclideanDomain a ⇒ (a → a) → Quotient a → Quotient a
liftQ f (Quotient Nothing x) = Quotient Nothing $ f x
liftQ f (Quotient (Just m) x) = proj m $ f x

-- | The associated binary function defined on the quotient. Definitely not guaranteed to be well defined (ie may depend on the choice of coset representative).
liftQ2' ∷ EuclideanDomain a ⇒ (a → a → b) → Quotient a → Quotient a → b
liftQ2' f (Quotient Nothing x)  (Quotient Nothing y)  = f x y
liftQ2' f (Quotient Nothing x)  (Quotient (Just m) y) = f (x `mod` m) y
liftQ2' f (Quotient (Just m) x) (Quotient Nothing y)  = f x (y `mod` m)
liftQ2' f (Quotient (Just m) x) (Quotient (Just m') y)
  | m =~ m' = f x y
  | otherwise = error "non matching moduli"

-- | The associated binary function defined on the quotient. Definitely not guaranteed to be well defined (ie may depend on the choice of coset representative).
liftQ2 ∷ EuclideanDomain a ⇒ (a → a → a) → Quotient a → Quotient a → Quotient a
liftQ2 f (Quotient Nothing x)  (Quotient Nothing y)  =  Quotient Nothing $ f x y
liftQ2 f (Quotient Nothing x)  (Quotient (Just m) y) = Quotient (Just m) $ f (x `mod` m) y
liftQ2 f (Quotient (Just m) x) (Quotient Nothing y)  = Quotient (Just m) $ f x (y `mod` m)
liftQ2 f (Quotient (Just m) x) (Quotient (Just m') y)
  | m =~ m' = proj m $ f x y
  | otherwise = error "non matching moduli"

------------------------------
---- Instances
------------------------------

instance Show a ⇒ Show (Quotient a) where
  show (Quotient Nothing x) = P.show x
  show (Quotient (Just m) x) = P.show x ++ " (mod " ++ P.show m ++ ")"

instance EuclideanDomain a ⇒ NumEq (Quotient a) where
  (=~) = liftQ2' (=~)
  epsilon = Quotient Nothing epsilon
  nearZero (Quotient Nothing x) = nearZero x
  nearZero (Quotient (Just m) x) = nearZero (x `mod` m)
  (>>~) = liftQ2' (>>~)

instance (EuclideanDomain a, Eq a) ⇒ Eq (Quotient a) where
  (==) = liftQ2' (==)

instance EuclideanDomain a ⇒ Monoid (Quotient a) where
  mempty = Quotient Nothing mempty
  mappend = liftQ2 mappend

instance EuclideanDomain a ⇒ Group (Quotient a) where
  --zero = zero :% one
  --(+) (x:%y) (x':%y') = (x*y' + x'*y) :% (y*y')
  negate = liftQ negate
  -- (-) (x:%y) (x':%y') = (x*y' - x'*y) :% (y*y')
instance EuclideanDomain a ⇒ Abelian (Quotient a) where

instance EuclideanDomain a ⇒ Ring (Quotient a) where
  one = Quotient Nothing one
  (*) = liftQ2 (*)

instance (Derivation a, EuclideanDomain a) ⇒ Derivation (Quotient  a) where
  derive = liftQ derive
