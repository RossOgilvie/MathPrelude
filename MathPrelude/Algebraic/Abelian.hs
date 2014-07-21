{-# LANGUAGE NoImplicitPrelude #-}
module MathPrelude.Algebraic.Abelian
	( module MathPrelude.Classes.NumEq
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
class (NumEq a, Monoid a) => Abelian a where
	-- zero :: a
	-- (+) :: a -> a -> a
	negate :: a -> a
	(-) :: a -> a -> a

	-- zero = mempty
	-- (+) = (<>)
	negate x = zero - x
	(-) x y = x + negate y

infixl 6  +, -

-----------------------------------
--- Methods
-----------------------------------
sum :: Monoid a => [a] -> a
sum = foldr mappend mempty

zero :: Monoid a => a
zero = mempty

(+) :: Monoid a => a -> a -> a
(+) = (<>)

-----------------------------------
--- Instances
-----------------------------------
instance Monoid Integer where mempty = zeroInteger; mappend = (P.+)
instance Abelian Integer where
	-- zero = mempty;
	-- (+) = (P.+);
	negate = P.negate;
	(-) = (P.-)

instance Monoid Int where mempty = zeroInt; mappend = (P.+)
instance Abelian Int where
	-- zero = mempty;
	-- (+) = (P.+);
	negate = P.negate;
	(-) = (P.-)

instance Monoid Int32 where mempty = zeroInt32; mappend = (P.+)
instance Abelian Int32 where
	-- zero = mempty;
	-- (+) = (P.+);
	negate = P.negate;
	(-) = (P.-)

instance Monoid Int64 where mempty = zeroInt64; mappend = (P.+)
instance Abelian Int64 where
	-- zero = mempty;
	-- (+) = (P.+);
	negate = P.negate;
	(-) = (P.-)

instance Monoid Float where mempty = zeroFloat; mappend = (P.+)
instance Abelian Float where
	-- zero = mempty;
	-- (+) = (P.+);
	negate = P.negate;
	(-) = (P.-)

instance Monoid Double where mempty = zeroDouble; mappend = (P.+)
instance Abelian Double where
	-- zero = mempty;
	-- (+) = (P.+);
	negate = P.negate;
	(-) = (P.-)

instance Abelian a => Abelian (Maybe a) where
	-- zero = Just zero
	-- (+) = liftM2 (+)
	(-) = liftM2 (-)
	negate = liftM negate