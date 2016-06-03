{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE UnicodeSyntax     #-}
-- | Represent a ring, a structure that is an abelian group with a unital multiplication operator that distributes over the group operation. Our rings are all commutative.
module MathPrelude.Classes.Ring
    ( module MathPrelude.Classes.Group
    , Ring(..)
    , Num(..)
    , CRing
    , IntDom
    , (^)
    , product
    -- , two
    ) where

-----------------------------------
--- Imports
-----------------------------------
import           BasicPrelude
import           PreludeNumConst
import qualified Prelude                     as P

import           MathPrelude.Classes.Group

-----------------------------------
--- Classes
-----------------------------------
-- | A ring is an abelian group with a multiplication operation that distributes over addition. This operation has an identity element. The is always a homomorphism from the ring of integers to any ring R, given by repeated addition (\n → 1 + .... + 1 or (-1) + ... + (-1)), (alternatively, a ring is just a Z-module).
class Abelian a ⇒ Ring a where
    -- | The identity element
    one ∷ a
    -- | The multiplication operation
    (*) ∷ a → a → a
    -- | Push an integer into the ring.
    fromInteger ∷ Integer → a

    fromInteger n
        | n < zeroInteger = negate (fi (negate n))
        | otherwise = fi n
            where
                fi m
                    | m =~ zeroInteger = zero
                    | m =~ oneInteger = one
                    | P.even m    = fin + fin
                    | otherwise = fin + fin + one
                        where fin = fi (m `P.div` twoInteger)

    {-# MINIMAL one, (*) #-}

infixl 7 *

-- | A compatibility class to replace the Prelude Num class.
class (Eq a, Show a, Ring a) ⇒ Num a  where
    abs, signum ∷ a → a

-- instance Num a ⇒ P.Num a

-- | A commutatice ring is one where the multiplication operation is commutative.
class Ring a ⇒ CRing a

-- | An integral domain is a ring with the property that non-zero elements multiply to give non-zero elements. Another way to say this is that there are no zero divisors. This is equivalent to the cancellation law holding.
class CRing a ⇒ IntDom a


-----------------------------------
-- Methods
-----------------------------------
-- | Fold a list together multiplicatively. An empty list yield the multiplicative identity
product ∷ Ring a ⇒ [a] → a
product = foldr (*) one

-- | Take the repeated product of an element of the ring. eg a^3 = a*a*a. Throws an error on negative powers.
(^) ∷ Ring a ⇒ a → Int → a
(^) x n
    | n < 0 = error "negative power"
    | n == 0 = one
    | otherwise = product $ zipWith f (intToBinary n) powers
        where
            powers = iterate (\a → a*a) x
            f b y = if b then y else one

infixr 8 ^

-- | converts an int to a list of binary digits. True= 1, False = 0
intToBinary ∷ Int → [Bool]
intToBinary n = reverse $ intToBinary' n powers
    where
        powers = reverse $ takeWhile (<= n) twopowers

intToBinary' ∷ Int → [Int] → [Bool]
intToBinary' _ [] = []
intToBinary' n (x:xs)
    | n >= x = True : intToBinary' (n-x) xs
    | otherwise = False : intToBinary' n xs

-- | The powers of 2
twopowers ∷ [Int]
twopowers = 1 : map (*2) twopowers

-- two ∷ Ring a ⇒ a
-- two = one + one

-----------------------------------
--- Instances
-----------------------------------

instance Ring Integer where one = oneInteger; (*) = (P.*); fromInteger = id;
instance Ring Int where one = oneInt; (*) = (P.*); fromInteger = P.fromInteger;
instance Ring Int32 where one = oneInt32; (*) = (P.*); fromInteger = P.fromInteger;
instance Ring Int64 where one = oneInt64; (*) = (P.*); fromInteger = P.fromInteger;
instance Ring Float where one = oneFloat; (*) = (P.*); fromInteger = P.fromInteger;
instance Ring Double where one = oneDouble; (*) = (P.*); fromInteger = P.fromInteger;

instance CRing Integer
instance CRing Int
instance CRing Int32
instance CRing Int64
instance CRing Float
instance CRing Double

instance IntDom Integer
instance IntDom Int
instance IntDom Int32
instance IntDom Int64
instance IntDom Float
instance IntDom Double

instance Num Integer where abs = P.abs; signum = P.signum
instance Num Int where abs = P.abs; signum = P.signum
instance Num Int32 where abs = P.abs; signum = P.signum
instance Num Int64 where abs = P.abs; signum = P.signum
instance Num Float where abs = P.abs; signum = P.signum
instance Num Double where abs = P.abs; signum = P.signum

instance Ring a ⇒ Ring (Maybe a) where one = Just one; (*) = liftM2 (*); fromInteger x = Just (fromInteger x)
instance CRing a ⇒ CRing (Maybe a)
instance IntDom a ⇒ IntDom (Maybe a)

instance Ring b ⇒ Ring (a → b) where
    one = const one
    (*) f g x = f x * g x
instance CRing b ⇒ CRing (a → b)
instance IntDom b ⇒ IntDom (a → b)
