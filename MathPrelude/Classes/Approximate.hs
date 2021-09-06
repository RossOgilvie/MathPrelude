-- {-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}

-- | A comparison operation that makes sense for numeric types, such as applying approximate equality for floats.
module MathPrelude.Classes.Approximate
    ( Approx(..)
    , nearZero
    )
where

import           MathPrelude.Prelude.CorePrelude
import           MathPrelude.Prelude.NamedNumbers
import qualified Prelude                       as P


-----------------------------------
--- Class
-----------------------------------
-- | A class providing comparison operations for non-exact numeric types. Typical example is floats. Exact types are also blessed with this, with a margin of error equal to zero.
class Approx a where
    -- | Numerically equal. Reduces to (==) for exact types, but useful for say Doubles
    (=~) ∷ a → a → Bool
    -- | Numerically not equal
    (/=~) ∷ a → a → Bool
    -- | Margin of error
    epsilon ∷ a

    (=~) x y = not $ (/=~) x y
    (/=~) x y = not $ (=~) x y

infixl 4 =~
infixl 4 /=~

-----------------------------------
--- Methods
-----------------------------------
-- | Test whether we are close to zero.
nearZero :: (Approx a, Monoid a) => a -> Bool
nearZero a = a =~ mempty

-- These are actually in the named numbers module
-- epsFloat = 1e-5 ∷ Float
-- epsDouble = 1e-10 ∷ Double

-----------------------------------
--- Instances
-----------------------------------
instance Approx Int where
    (=~)    = (==)
    epsilon = zeroInt
instance Approx Int32 where
    (=~)    = (==)
    epsilon = zeroInt32
instance Approx Int64 where
    (=~)    = (==)
    epsilon = zeroInt64
instance Approx Integer where
    (=~)    = (==)
    epsilon = zeroInteger

instance Approx Float where
    (=~) x y =
        P.abs (x P.- y) <= epsFloat P.* P.maximum [oneFloat, P.abs x, P.abs y]
    epsilon = epsFloat
instance Approx Double where
    (=~) x y =
        P.abs (x P.- y) <= epsDouble P.* P.maximum [oneDouble, P.abs x, P.abs y]
    epsilon = epsDouble

instance Approx a ⇒ Approx [a] where
    (=~) x y = length x == length y && and (zipWith (=~) x y)
    epsilon = [epsilon]

instance (Approx a, Approx b) ⇒ Approx (a,b) where
    (a1, b1) =~ (a2, b2) = a1 =~ a2 && b1 =~ b2
    epsilon = (epsilon, epsilon)
instance (Approx a, Approx b, Approx c) ⇒ Approx (a,b,c) where
    (a1, b1, c1) =~ (a2, b2, c2) = a1 =~ a2 && b1 =~ b2 && c1 =~ c2
    epsilon = (epsilon, epsilon, epsilon)
instance (Approx a, Approx b, Approx c, Approx d) ⇒ Approx (a,b,c,d) where
    (a1, b1, c1, d1) =~ (a2, b2, c2, d2) =
        a1 =~ a2 && b1 =~ b2 && c1 =~ c2 && d1 =~ d2
    epsilon = (epsilon, epsilon, epsilon, epsilon)
instance (Approx a, Approx b, Approx c, Approx d, Approx e) ⇒ Approx (a,b,c,d,e) where
    (a1, b1, c1, d1, e1) =~ (a2, b2, c2, d2, e2) =
        a1 =~ a2 && b1 =~ b2 && c1 =~ c2 && d1 =~ d2 && e1 =~ e2
    epsilon = (epsilon, epsilon, epsilon, epsilon, epsilon)

instance Approx a ⇒ Approx (Maybe a) where
    (=~) (Just x) (Just y) = x =~ y
    (=~) Nothing  Nothing  = True
    (=~) _        _        = False
    epsilon = Just epsilon
