{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE UnicodeSyntax         #-}
module MathPrelude.Constructions.Ratio
    ( module MathPrelude.Classes.Field
    , Ratio(..)
    , numerator
    , denominator
    , simplifyR
    , evaluateR
    )  where

import           BasicPrelude
import qualified Prelude                             as P

import           MathPrelude.Classes.EuclideanDomain
import           MathPrelude.Classes.Field

------------------------------
--- Ratio
------------------------------
-- | Representing the ratio of two elements. Algebraically, this is an element of the field of fractions.
data Ratio a = a :% a deriving Show

-- | Return the numerator (the top number).
numerator ∷ Ratio a → a
numerator (a :% b) = a
-- | Return the denominator (the bottom number).
denominator ∷ Ratio a → a
denominator (a :% b) = b

-- | Calculates the canonical choice of sign.
parity' ∷ Ordering → Ordering → Ordering → Ordering
parity' EQ _ _ = EQ
parity' x y z
    | y == z = x
    | y /= z = opposite x
        where
            opposite LT = GT
            opposite GT = LT

-- | Reduce the ratio to one of coprime numbers.
simplifyR ∷ EuclideanDomain a ⇒ Ratio a → Ratio a
simplifyR (p:%q) = (p `div` g) :% (q `div` g)
    where g = gcd p q

-- | Evaluate a ratio into a field.
evaluateR ∷ Field a ⇒ Ratio a → a
evaluateR (x :% y) = x/y

------------------------------
---- Instances
------------------------------
instance Functor Ratio where
    fmap f (x:%y) = f x :% f y

instance (IntDom a, Eq a) ⇒ Eq (Ratio a) where
    (==) p@(x:%_) q@(y:%_)
        | x == zero && y == zero = True
        | otherwise = (p-q) == zero
instance (IntDom a, Ord a) ⇒ Ord (Ratio a) where
    compare (x:%y) (x':%y') = parity' num yord y'ord
        where
            num = compare (x*y' - x'*y) zero
            yord = compare y zero
            y'ord = compare y' zero

instance (IntDom a, NumEq a) ⇒ NumEq (Ratio a) where
    (=~) (x:%y) (x':%y') = (x*y' - x'*y) =~ 0
    epsilon = epsilon :% one
    -- (=~) (x:%y) (x':%y') = smallL [x,y,x',y'] (x*y' - x'*y)
    -- nearZero = (>>~) zero
    -- (>>~) (x:%y) (x':%y') = (>>~) (x*y') (x'*y)


instance IntDom a ⇒ Monoid (Ratio a) where
    mempty = zero :% one
    mappend (x:%y) (x':%y') = (x*y' + x'*y) :% (y*y')

instance IntDom a ⇒ Group (Ratio a) where
    negate (x:%y) = negate x :% y
    (-) (x:%y) (x':%y') = (x*y' - x'*y) :% (y*y')
instance IntDom a ⇒ Abelian (Ratio a)

instance IntDom a ⇒ Ring (Ratio a) where
    one = one :% one
    (*) (x:%y) (x':%y') = (x*x') :% (y*y')

instance IntDom a ⇒ CRing (Ratio a)

instance IntDom a ⇒ IntDom (Ratio a)

instance IntDom a ⇒ Field (Ratio a) where
    recip (x :% y) = y :% x
    (/) (x:%y) (x':%y') = (x*y') :% (y*x')
