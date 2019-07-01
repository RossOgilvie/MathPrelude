{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}
-- | A special ring that has a division procedure. This procedure allows the implementation of the Euclidean algorithm.
module MathPrelude.Classes.EuclideanDomain
    ( module MathPrelude.Classes.Ring
    , EuclideanDomain(..)
    , gcd, gcd'
    , extEuclidAlg
    ) where

import           BasicPrelude
import qualified Prelude                  as P

import           MathPrelude.Classes.Ring

-----------------------------------
--- EuclideanDomain
-----------------------------------
-- | A Euclidean Domain is an integral domain with a division algorithm that respects some norm.
class IntDom a ⇒ EuclideanDomain a where
    -- | Whole division. div a b = q where a = q*b + r
    div ∷ a → a → a
    -- | Remainder, aka modulo. mod a b = r where a = q*b + r
    mod ∷ a → a → a
    -- | Both division and remainder. divMod a b = (d,r) where a = q*b + r
    divMod ∷ a → a → (a,a)

    -- | In a ring where elements have a preferred associate, this gives that associate
    stdAssociate ∷ a → a
    -- | The standard unit satisfies the property that x = stdAssoc * stdUnit.
    stdUnit ∷ a → a
    -- | A combination of (stdAssoc, stdUnit)
    normalize ∷ a → (a, a)

    stdAssociate x  =  x `div` stdUnit x
    stdUnit x       =  snd (normalize x)
    normalize x     =  (stdAssociate x, stdUnit x)

    n `divMod` d    =  (n `div` d, n `mod` d)
    n `div` d       =  fst $ divMod n d
    n `mod` d       =  snd $ divMod n d

    {-# MINIMAL (stdUnit | normalize), (divMod | (div, mod))  #-}


-----------------------------------
--- Methods
-----------------------------------
-- | The greatest common divisor of two elements, presented as the standard associate.
gcd ∷ (EuclideanDomain a, Eq a) ⇒ a → a → a
gcd a b = stdAssociate $ gcd' a b
-- | The greatest common divisor of two elements, presented as is.
gcd' a b
    | b == zero = a
    | otherwise = gcd' b (a `mod` b)

-- | The extended Euclidean algorithm returns two numbers (x,y) from input (a,b) such that x*a + y*b = gcd'(a,b).
extEuclidAlg ∷ (EuclideanDomain a, Eq a) ⇒ a → a → (a,a)
extEuclidAlg a b
    | r == zero = (zero,one)
    | otherwise = (y, x - (y * q))
        where
            (q,r) = a `divMod` b
            (x,y) = extEuclidAlg b r


-----------------------------------
--- Instances
-----------------------------------
instance EuclideanDomain Integer where stdAssociate = P.abs; stdUnit = P.signum; divMod = P.divMod
instance EuclideanDomain Int where stdAssociate = P.abs;stdUnit = P.signum; divMod = P.divMod;
instance EuclideanDomain Int32 where stdAssociate = P.abs; stdUnit = P.signum; divMod = P.divMod
instance EuclideanDomain Int64 where stdAssociate = P.abs; stdUnit = P.signum; divMod = P.divMod
instance EuclideanDomain Word where stdAssociate = P.abs;stdUnit = P.signum; divMod = P.divMod;
instance EuclideanDomain Word32 where stdAssociate = P.abs; stdUnit = P.signum; divMod = P.divMod
instance EuclideanDomain Word64 where stdAssociate = P.abs; stdUnit = P.signum; divMod = P.divMod

instance EuclideanDomain a ⇒ EuclideanDomain (Maybe a) where
    stdAssociate = liftM stdAssociate
    stdUnit = liftM stdUnit
    div = liftM2 div
    mod = liftM2 mod
