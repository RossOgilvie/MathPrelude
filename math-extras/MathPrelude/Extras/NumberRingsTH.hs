{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MathPrelude.Extras.NumberRingsTH where

--------------------------------------
-- Imports
--------------------------------------
import MathPrelude hiding (Q)

-- From math-prelude
import MathPrelude.Classes.Field
import MathPrelude.Classes.EuclideanDomain
import MathPrelude.Classes.Ring

-- TH stuff
import GHC.TypeLits
import Data.Proxy
import Language.Haskell.TH

-- The primes to make finite fields for
primes ∷ [Integer]
primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101]

--------------------------------------
-- Methods
--------------------------------------
-- | Find a reciprocal in a finite field using the extended Euclidean algorithm.
findRecip m z = fst $ extEuclidAlg z m

data Z (n ∷ Nat) = Z Integer

modulusZ ∷ forall n. KnownNat n ⇒ Z n → Integer
modulusZ _ = natVal (Proxy ∷ Proxy n)

liftZ ∷ KnownNat n ⇒ (Integer → Integer) → Z n → Z n
liftZ f z@(Z a) = Z $ f a `mod` modulusZ z

liftZ2 ∷ KnownNat n ⇒ (Integer → Integer → Integer) → Z n → Z n → Z n
liftZ2 f z@(Z a) (Z b) = Z $ f a b `mod` modulusZ z

liftZ2' ∷ KnownNat n ⇒ (Integer → Integer → c) → Z n → Z n → c
liftZ2' f (Z a) (Z b) = f a b


--------------------------------------
-- TH
--------------------------------------

monadConcat ∷ Monad m ⇒ [m [a]] → m [a]
monadConcat [] = return []
monadConcat (x:xs) = do
  x' <- x
  xs' <- monadConcat xs
  return (x' ++ xs')

mkFiniteFields ∷ [Integer] → Q [Dec]
mkFiniteFields ns = monadConcat (map mkIntDom ns ++ map mkField ns)

mkIntDom :: Integer -> Q [Dec]
mkIntDom n = return [InstanceD Nothing [] (AppT (ConT ''IntDom) (AppT (ConT ''Z) (LitT (NumTyLit n)))) []]

mkField :: Integer -> Q [Dec]
mkField n = do
  a <- newName "a"
  z <- newName "z"
  return [InstanceD Nothing [] (AppT (ConT ''Field) (AppT (ConT ''Z) (LitT (NumTyLit n)))) [FunD 'recip [Clause [VarP z] (NormalB (AppE (AppE (VarE 'liftZ) (AppE (VarE 'findRecip) (AppE (VarE 'modulusZ) (VarE z)))) (VarE z))) []]]]
