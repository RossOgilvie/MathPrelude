{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MathPrelude.Extras.NumberRingsTH where

import BasicPrelude

import MathPrelude.Algebraic.Ring
import MathPrelude.Algebraic.Field
import MathPrelude.Algebraic.EuclideanDomain

import Language.Haskell.TH

import GHC.TypeLits
import Data.Proxy

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
mkIntDom n = return [InstanceD [] (AppT (ConT ''IntDom) (AppT (ConT ''Z) (LitT (NumTyLit n)))) []]

mkField :: Integer -> Q [Dec]
mkField n = do
  a <- newName "a"
  z <- newName "z"
  return [InstanceD [] (AppT (ConT ''Field) (AppT (ConT ''Z) (LitT (NumTyLit n)))) [FunD 'recip [Clause [VarP z] (NormalB (AppE (AppE (VarE 'liftZ) (AppE (VarE 'findRecip) (AppE (VarE 'modulusZ) (VarE z)))) (VarE z))) []]]]