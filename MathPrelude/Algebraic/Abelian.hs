{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
module MathPrelude.Algebraic.Abelian
	( module MathPrelude.Classes.NumEq
	, Group(..)
	, Abelian(..)
	, zero
	, (+)
	, sum) where

-----------------------------------
--- Imports
-----------------------------------
import BasicPrelude
import qualified Prelude as P

import MathPrelude.Classes.NumEq
import MathPrelude.Common.PreludeNumConst

-----------------------------------
--- Classes
-----------------------------------
class (NumEq a, Monoid a) => Group a where

	negate :: a -> a
	(-) :: a -> a -> a

	negate x = zero - x
	(-) x y = x <> negate y

class Group a ⇒ Abelian a

infixl 6 -

-----------------------------------
--- Methods
-----------------------------------
(+) :: Abelian a => a -> a -> a
(+) = (<>)
infixl 6 +

zero :: Monoid a => a
zero = mempty

sum :: Monoid a => [a] -> a
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

instance Group a => Group (Maybe a) where
	(-) = liftM2 (-)
	negate = liftM negate
instance Abelian a ⇒ Abelian (Maybe a)
