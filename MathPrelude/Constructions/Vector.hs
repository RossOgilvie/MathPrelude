{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses#-}

module MathPrelude.Constructions.Vector
  ( Vec()
  , dimV
  , liftV, liftV2
  , vectorL, toListV, fromListV
  , vectorT1, vectorT2, vectorT3, vectorT4, vectorT5
  , basisV, basisV1, basisV2, basisV3, basisV4, basisV5
  , dotV
  -- , iprod
  , crossV
  -- , normV, unitizeV
  ) where

-----------------------------------
--- Imports
-----------------------------------
import BasicPrelude hiding (foldr)
import qualified Prelude as P

import MathPrelude.Classes.Module
import MathPrelude.Classes.Field
import MathPrelude.Classes.Integral
-- import MathPrelude.Classes.Real
import MathPrelude.Classes.Transcendental
import MathPrelude.Classes.Norm

import MathPrelude.Constructions.Complex

import GHC.TypeLits
import Data.Proxy

import Control.Lens
-- import Control.Lens.Tuple

import qualified Data.Foldable as F

-----------------------------------
--- Vec
-----------------------------------
data Vec (n ∷ Nat) a = Vec [a]

type V0 a = Vec 0 a
type V1 a = Vec 1 a
type V2 a = Vec 2 a
type V3 a = Vec 3 a
type V4 a = Vec 4 a
type V5 a = Vec 5 a

-----------------------------------
--- Helpers
-----------------------------------
dimV ∷ forall n a. KnownNat n ⇒ Vec n a → Integer
dimV _ = natVal (Proxy ∷ Proxy n)

liftV ∷ (a→b) → Vec n a → Vec n b
liftV f (Vec as) = Vec (map f as)

liftV2 ∷ (a → b → c) → Vec n a → Vec n b → Vec n c
liftV2 f (Vec as) (Vec bs) = Vec (zipWith f as bs)

-----------------------------------
--- Constructors
-----------------------------------

vectorL :: forall n a. KnownNat n ⇒ Iso' [a] (Vec n a)
vectorL = iso fromListV toListV
{-# INLINE vectorL #-}

toListV ∷ Vec n a → [a]
toListV (Vec as) = as

fromListV ∷ forall n a. KnownNat n ⇒ [a] → Vec n a
fromListV as = if b then Vec as' else error "fromListV: supplied list not long enough."
  where
    n' = natVal (Proxy ∷ Proxy n)
    (b, as') = takeAndCountV n' as

takeAndCountV ∷ Integer → [a] → (Bool, [a])
takeAndCountV n xs = let n' = view integral n; xs' = take n' xs in (length xs' == n', xs')

vectorT1 ∷ Iso' a (Vec 1 a)
vectorT1 = iso fromTuple1 toTuple1
{-# INLINE vectorT1 #-}

vectorT2 ∷ Iso' (a,a) (Vec 2 a)
vectorT2 = iso fromTuple2 toTuple2
{-# INLINE vectorT2 #-}

vectorT3 ∷ Iso' (a,a,a) (Vec 3 a)
vectorT3 = iso fromTuple3 toTuple3
{-# INLINE vectorT3 #-}

vectorT4 ∷ Iso' (a,a,a,a) (Vec 4 a)
vectorT4 = iso fromTuple4 toTuple4
{-# INLINE vectorT4 #-}

vectorT5 ∷ Iso' (a,a,a,a,a) (Vec 5 a)
vectorT5 = iso fromTuple5 toTuple5
{-# INLINE vectorT5 #-}

fromTuple1 ∷ (a) → Vec 1 a
fromTuple1 (x) = Vec [x]

fromTuple2 ∷ (a,a) → Vec 2 a
fromTuple2 (x,y) = Vec [x,y]

fromTuple3 ∷ (a,a,a) → Vec 3 a
fromTuple3 (x,y,z) = Vec [x,y,z]

fromTuple4 ∷ (a,a,a,a) → Vec 4 a
fromTuple4 (x,y,z,u) = Vec [x,y,z,u]

fromTuple5 ∷ (a,a,a,a,a) → Vec 5 a
fromTuple5 (x,y,z,u,v) = Vec [x,y,z,u,v]

toTuple1 ∷ Vec 1 a → a
toTuple1 (Vec [x]) = (x)

toTuple2 ∷ Vec 2 a → (a,a)
toTuple2 (Vec [x,y]) = (x,y)

toTuple3 ∷ Vec 3 a → (a,a,a)
toTuple3 (Vec [x,y,z]) = (x,y,z)

toTuple4 ∷ Vec 4 a → (a,a,a,a)
toTuple4 (Vec [x,y,z,u]) = (x,y,z,u)

toTuple5 ∷ Vec 5 a → (a,a,a,a,a)
toTuple5 (Vec [x,y,z,u,v]) = (x,y,z,u,v)

basisV ∷ forall n a. (KnownNat n, Ring a) ⇒ Integer → Vec n a
basisV i = Vec $ r (i'-1) ++ [1] ++ r (n'-i')
  where
    n' = natVal (Proxy ∷ Proxy n)
    i' = min n' . max 0 $ i
    r k = take (fromInteger k) . repeat $ 0

basisV1 ∷ Ring a ⇒ Integer → Vec 1 a
basisV1 1 = Vec [1]

basisV2 ∷ Ring a ⇒ Integer → Vec 2 a
basisV2 1 = Vec [1,0]
basisV2 2 = Vec [0,1]

basisV3 ∷ Ring a ⇒ Integer → Vec 3 a
basisV3 1 = Vec [1,0,0]
basisV3 2 = Vec [0,1,0]
basisV3 3 = Vec [0,0,1]

basisV4 ∷ Ring a ⇒ Integer → Vec 4 a
basisV4 1 = Vec [1,0,0,0]
basisV4 2 = Vec [0,1,0,0]
basisV4 3 = Vec [0,0,1,0]
basisV4 4 = Vec [0,0,0,1]

basisV5 ∷ Ring a ⇒ Integer → Vec 5 a
basisV5 1 = Vec [1,0,0,0,0]
basisV5 2 = Vec [0,1,0,0,0]
basisV5 3 = Vec [0,0,1,0,0]
basisV5 4 = Vec [0,0,0,1,0]
basisV5 5 = Vec [0,0,0,0,1]

-----------------------------------
--- Operations
-----------------------------------
-- instance (KnownNat n, Ring a) ⇒ InnerProd (Vec n a) a where
  -- iprod v w = sum . toListV $ liftV2 (*) v w
dotV ∷ Ring a ⇒ Vec n a → Vec n a → a
dotV v w = sum . toListV $ liftV2 (*) v w

crossV ∷ Ring a ⇒ Vec 3 a → Vec 3 a → Vec 3 a
crossV (Vec [a1,a2,a3]) (Vec [b1,b2,b3]) = Vec [a2*b3-a3*b2, a3*b1-a1*b3, a1*b2 - a2*b1]

-- normV ∷ Transcendental a ⇒ Vec n a → a
-- normV = sqrt . sum . map (^2) . toListV

-- unitizeV ∷ (KnownNat n, Transcendental a) ⇒ Vec n a → Vec n a
-- unitizeV v = normV v ./ v

-----------------------------------
--- Instances
-----------------------------------
instance Show a ⇒ Show (Vec n a) where
  show (Vec as) = "V " ++ P.show as

instance Functor (Vec n) where
  fmap f (Vec as) = Vec . fmap f $ as

instance F.Foldable (Vec n) where
    {-# INLINE F.foldr #-}
    foldr f x (Vec xs) = F.foldr f x xs

instance Traversable (Vec n) where
    {-# INLINE traverse #-}
    traverse f (Vec xs) = Vec <$> traverse f xs

-----------------------------------
--- Math Instances
-----------------------------------
instance (KnownNat n, Monoid a) ⇒ Monoid (Vec n a) where
  mappend = liftV2 mappend
  mempty = Vec (take n' $ repeat mempty)
    where n' = fromInteger $ natVal (Proxy ∷ Proxy n)

instance NumEq a ⇒ NumEq (Vec n a) where
  (=~) x = and . toListV . liftV2 (=~) x

instance (KnownNat n, Group a) ⇒ Group (Vec n a) where
  negate = liftV negate

instance (KnownNat n, Abelian a) ⇒ Abelian (Vec n a)

instance (KnownNat n, Ring a) ⇒ Module (Vec n a) a where
  scale b = liftV (b*)

instance (KnownNat n, Field a) ⇒ VectorSpace (Vec n a) a

instance KnownNat n ⇒ InnerProd (Vec n Double) Double where
  iprod = dotV

instance KnownNat n ⇒ Norm (Vec n Double) Double where
  norm v = sqrt $ dotV v v

instance (KnownNat n, Ring a, InnerProd (Vec n a) a) ⇒ InnerProd (Vec n (Complex a)) (Complex a) where
  iprod v w = dotV v (liftV conjugate w)

instance (KnownNat n, Transcendental a, InnerProd (Vec n (Complex a)) (Complex a)) ⇒ Norm (Vec n (Complex a)) a where
  norm v = sqrt . realPart $ iprod v v

-- instance (KnownNat n, ComplexClass (Complex a), Transcendental a) ⇒ Norm (Vec n (Complex a)) a where
--   norm v = sqrt . realPart $ dotV v v


-----------------------------------
--- Lens Instances
-----------------------------------
instance Field1 (Vec 1 a) (Vec 1 a') a a' where
  _1 k ~(Vec [a]) = k a <&> \a' -> Vec [a']
  {-# INLINE _1 #-}

instance Field1 (Vec 2 a) (Vec 2 a) a a where
  _1 k ~(Vec [a,b]) = k a <&> \a' -> Vec [a',b]
  {-# INLINE _1 #-}

instance Field1 (Vec 3 a) (Vec 3 a) a a where
  _1 k ~(Vec [a,b,c]) = k a <&> \a' -> Vec [a',b,c]
  {-# INLINE _1 #-}

instance Field1 (Vec 4 a) (Vec 4 a) a a where
  _1 k ~(Vec [a,b,c,d]) = k a <&> \a' -> Vec [a',b,c,d]
  {-# INLINE _1 #-}

instance Field1 (Vec 5 a) (Vec 5 a) a a where
  _1 k ~(Vec [a,b,c,d,e]) = k a <&> \a' -> Vec [a',b,c,d,e]
  {-# INLINE _1 #-}


instance Field2 (Vec 2 a) (Vec 2 a) a a where
  _2 k ~(Vec [a,b]) = k b <&> \b' -> Vec [a,b']
  {-# INLINE _2 #-}

instance Field2 (Vec 3 a) (Vec 3 a) a a where
  _2 k ~(Vec [a,b,c]) = k b <&> \b' -> Vec [a,b',c]
  {-# INLINE _2 #-}

instance Field2 (Vec 4 a) (Vec 4 a) a a where
  _2 k ~(Vec [a,b,c,d]) = k b <&> \b' -> Vec [a,b',c,d]
  {-# INLINE _2 #-}

instance Field2 (Vec 5 a) (Vec 5 a) a a where
  _2 k ~(Vec [a,b,c,d,e]) = k b <&> \b' -> Vec [a,b',c,d,e]
  {-# INLINE _2 #-}


instance Field3 (Vec 3 a) (Vec 3 a) a a where
  _3 k ~(Vec [a,b,c]) = k c <&> \c' -> Vec [a,b,c']
  {-# INLINE _3 #-}

instance Field3 (Vec 4 a) (Vec 4 a) a a where
  _3 k ~(Vec [a,b,c,d]) = k c <&> \c' -> Vec [a,b,c',d]
  {-# INLINE _3 #-}

instance Field3 (Vec 5 a) (Vec 5 a) a a where
  _3 k ~(Vec [a,b,c,d,e]) = k c <&> \c' -> Vec [a,b,c',d,e]
  {-# INLINE _3 #-}


instance Field4 (Vec 4 a) (Vec 4 a) a a where
  _4 k ~(Vec [a,b,c,d]) = k d <&> \d' -> Vec [a,b,c,d']
  {-# INLINE _4 #-}

instance Field4 (Vec 5 a) (Vec 5 a) a a where
  _4 k ~(Vec [a,b,c,d,e]) = k d <&> \d' -> Vec [a,b,c,d',e]
  {-# INLINE _4 #-}


instance Field5 (Vec 5 a) (Vec 5 a) a a where
  _5 k ~(Vec [a,b,c,d,e]) = k e <&> \e' -> Vec [a,b,c,d,e']
  {-# INLINE _5 #-}
