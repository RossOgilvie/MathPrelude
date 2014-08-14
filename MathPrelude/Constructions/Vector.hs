{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses#-}
module MathPrelude.Constructions.Vector
  ( Vec()
  , dimV
  , liftV, liftV2
  , toListV, fromListV
  , fromTupleV1, fromTupleV2, fromTupleV3, fromTupleV4, fromTupleV5
  , basisV1, basisV2, basisV3, basisV4, basisV5
  , dotV, crossV
  ) where

-----------------------------------
--- Imports
-----------------------------------
import BasicPrelude hiding (Vec)
import qualified Prelude as P

import MathPrelude.Algebraic.Module
import MathPrelude.Algebraic.Field

import GHC.TypeLits
import Data.Proxy
-----------------------------------
--- Vec
-----------------------------------
data Vec (n ∷ Nat) a = V [a]

-----------------------------------
--- Helpers
-----------------------------------
dimV ∷ forall n a. KnownNat n ⇒ Vec n a → Integer
dimV _ = natVal (Proxy ∷ Proxy n)

liftV ∷ (a→b) → Vec n a → Vec n b
liftV f (V as) = V (map f as)

liftV2 ∷ (a → b → c) → Vec n a → Vec n b → Vec n c
liftV2 f (V as) (V bs) = V (zipWith f as bs)

toListV ∷ Vec n a → [a]
toListV (V as) = as

-----------------------------------
--- Constructors
-----------------------------------
takeAndCount ∷ Integer → [a] → (Bool, [a])
takeAndCount n [] = (n <= 0, [])
takeAndCount n (x:xs)
  | n <= 0 = (True, [])
  | otherwise = let (b, ys) = takeAndCount (n-1) xs in (b, x:ys)

fromListV ∷ forall n a. KnownNat n ⇒ [a] → Vec n a
fromListV as = if b then V as' else error "fromListV: supplied list not long enough."
  where
    n' = natVal (Proxy ∷ Proxy n)
    (b, as') = takeAndCount n' as

fromTupleV1 ∷ (a) → Vec 1 a
fromTupleV1 (x) = V [x]

fromTupleV2 ∷ (a,a) → Vec 2 a
fromTupleV2 (x,y) = V [x,y]

fromTupleV3 ∷ (a,a,a) → Vec 3 a
fromTupleV3 (x,y,z) = V [x,y,z]

fromTupleV4 ∷ (a,a,a,a) → Vec 4 a
fromTupleV4 (x,y,z,u) = V [x,y,z,u]

fromTupleV5 ∷ (a,a,a,a,a) → Vec 5 a
fromTupleV5 (x,y,z,u,v) = V [x,y,z,u,v]

basisV1 ∷ Ring a ⇒ Integer → Vec 1 a
basisV1 1 = V [1]

basisV2 ∷ Ring a ⇒ Integer → Vec 2 a
basisV2 1 = V [1,0]
basisV2 2 = V [0,1]

basisV3 ∷ Ring a ⇒ Integer → Vec 3 a
basisV3 1 = V [1,0,0]
basisV3 2 = V [0,1,0]
basisV3 3 = V [0,0,1]

basisV4 ∷ Ring a ⇒ Integer → Vec 4 a
basisV4 1 = V [1,0,0,0]
basisV4 2 = V [0,1,0,0]
basisV4 3 = V [0,0,1,0]
basisV4 4 = V [0,0,0,1]

basisV5 ∷ Ring a ⇒ Integer → Vec 5 a
basisV5 1 = V [1,0,0,0,0]
basisV5 2 = V [0,1,0,0,0]
basisV5 3 = V [0,0,1,0,0]
basisV5 4 = V [0,0,0,1,0]
basisV5 5 = V [0,0,0,0,1]

-----------------------------------
--- Operations
-----------------------------------
dotV ∷ Ring a ⇒ Vec n a → Vec n a → a
dotV v = sum . toListV . liftV2 (*) v

crossV ∷ Ring a ⇒ Vec 3 a → Vec 3 a → Vec 3 a
crossV (V [a1,a2,a3]) (V [b1,b2,b3]) = V [a2*b3-a3*b2, a3*b1-a1*b3, a1*b2 - a2*b1]

-----------------------------------
--- Instances
-----------------------------------
instance Show a ⇒ Show (Vec n a) where
  show (V as) = "V " ++ P.show as

instance Functor (Vec n) where
  fmap f (V as) = V . fmap f $ as

instance (KnownNat n, Monoid a) ⇒ Monoid (Vec n a) where
  mappend = liftV2 mappend
  mempty = V (take n' $ repeat mempty)
    where n' = fromInteger $ natVal (Proxy ∷ Proxy n)

instance NumEq a ⇒ NumEq (Vec n a) where
  (=~) x = and . toListV . liftV2 (=~) x

instance (KnownNat n, Group a) ⇒ Group (Vec n a) where
  negate = liftV negate

instance (KnownNat n, Abelian a) ⇒ Abelian (Vec n a)

instance (KnownNat n, Ring a) ⇒ Module (Vec n a) a where
  scale b = liftV (b*)
