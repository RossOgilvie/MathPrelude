{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}
-- | Represent a group, a structure with an invertible operation, that may or may not be commutative (aka abelian).
module MathPrelude.Classes.Group
	( module MathPrelude.Classes.NumEq
	, Group(..)
	, Abelian
	, zero
	, (+)
	, sum) where

-----------------------------------
--- Imports
-----------------------------------
import           PreludeNumConst
import           BasicPrelude
import qualified Prelude                     as P

import           MathPrelude.Classes.NumEq

-----------------------------------
--- Classes
-----------------------------------
-- | This class defines a group. It extends the monoid structure by allowing negation. Minimal definition is either 'negate' or '-'
class (NumEq a, Monoid a) ⇒ Group a where
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
instance Monoid Integer where mempty = zeroInteger; mappend = (P.+)
instance Group Integer where
	negate = P.negate;
	(-) = (P.-)
instance Abelian Integer

instance Monoid Int where mempty = zeroInt; mappend = (P.+)
instance Group Int where
	negate = P.negate;
	(-) = (P.-)
instance Abelian Int

instance Monoid Int32 where mempty = zeroInt32; mappend = (P.+)
instance Group Int32 where
	negate = P.negate;
	(-) = (P.-)
instance Abelian Int32

instance Monoid Int64 where mempty = zeroInt64; mappend = (P.+)
instance Group Int64 where
	negate = P.negate;
	(-) = (P.-)
instance Abelian Int64

instance Monoid Float where mempty = zeroFloat; mappend = (P.+)
instance Group Float where
	negate = P.negate;
	(-) = (P.-)
instance Abelian Float

instance Monoid Double where mempty = zeroDouble; mappend = (P.+)
instance Group Double where
	negate = P.negate;
	(-) = (P.-)
instance Abelian Double

instance Group a ⇒ Group (Maybe a) where
	(-) = liftM2 (-)
	negate = liftM negate
instance Abelian a ⇒ Abelian (Maybe a)

-- instance Monoid b ⇒ Monoid (a→b) where
-- 	mempty = const mempty
-- 	mappend f g x = mappend (f x) (g x)
instance NumEq b ⇒ NumEq (a→b) where
instance Group b ⇒ Group (a→b) where
	negate f x = negate (f x)
instance Abelian b ⇒ Abelian (a→b)
