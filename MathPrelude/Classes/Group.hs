{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}

-- | Represent a group, a structure with an invertible operation, that may or may not be commutative (aka abelian).
module MathPrelude.Classes.Group
    ( Group(..)
    , Abelian
    , zero
    , (+)
    , sum) where

-----------------------------------
--- Imports
-----------------------------------
import           MathPrelude.Prelude.NamedNumbers
import           MathPrelude.Prelude.CorePrelude
import qualified Prelude                     as P

-----------------------------------
--- Classes
-----------------------------------
-- | This class defines a group. It extends the monoid structure by allowing negation. Minimal definition is either 'negate' or '-'
class Monoid a ⇒ Group a where
    negate ∷ a → a
    (-) ∷ a → a → a

    negate x = zero - x
    (-) x y = x <> negate y

    {-# MINIMAL negate | (-)  #-}

infixl 6 -

-- | This class blesses a group to say that it is abelian. Abelian groups are ones with a commutative operation.
class Group a ⇒ Abelian a

-----------------------------------
--- Methods
-----------------------------------
-- | Provides a synonym for the group operation, that may only be used if the group has been declared abelian.
(+) ∷ Abelian a ⇒ a → a → a
(+) = (<>)
infixl 6 +

-- | Provides a synonym for the group identity.
zero ∷ Monoid a ⇒ a
zero = mempty

-- | Fold a list together with the monoid operation.
sum ∷ Monoid a ⇒ [a] → a
sum = foldr mappend mempty

-----------------------------------
--- Instances
-----------------------------------
instance Semigroup Integer where (<>) = (P.+)
instance Monoid Integer where mempty = zeroGeneric 
instance Group Integer where negate = P.negate; (-) = (P.-)
instance Abelian Integer

instance Semigroup Int where (<>) = (P.+)
instance Monoid Int where mempty = zeroGeneric
instance Group Int where negate = P.negate; (-) = (P.-)
instance Abelian Int

instance Semigroup Int32 where (<>) = (P.+)
instance Monoid Int32 where mempty = zeroGeneric
instance Group Int32 where negate = P.negate; (-) = (P.-)
instance Abelian Int32

instance Semigroup Int64 where (<>) = (P.+)
instance Monoid Int64 where mempty = zeroGeneric
instance Group Int64 where negate = P.negate; (-) = (P.-)
instance Abelian Int64

instance Semigroup Word where (<>) = (P.+)
instance Monoid Word where mempty = zeroGeneric
instance Group Word where negate = P.negate; (-) = (P.-)
instance Abelian Word

instance Semigroup Word32 where (<>) = (P.+)
instance Monoid Word32 where mempty = zeroGeneric
instance Group Word32 where negate = P.negate; (-) = (P.-)
instance Abelian Word32

instance Semigroup Word64 where (<>) = (P.+)
instance Monoid Word64 where mempty = zeroGeneric
instance Group Word64 where negate = P.negate; (-) = (P.-)
instance Abelian Word64

instance Semigroup Float where (<>) = (P.+)
instance Monoid Float where mempty = zeroGeneric
instance Group Float where negate = P.negate; (-) = (P.-)
instance Abelian Float

instance Semigroup Double where (<>) = (P.+)
instance Monoid Double where mempty = zeroGeneric
instance Group Double where negate = P.negate; (-) = (P.-)
instance Abelian Double

instance Group a ⇒ Group (Maybe a) where
    (-) = liftM2 (-)
    negate = liftM negate
instance Abelian a ⇒ Abelian (Maybe a)

-- instance Group b ⇒ Group (a→b) where
--     negate f x = negate (f x)
-- instance Abelian b ⇒ Abelian (a→b)
