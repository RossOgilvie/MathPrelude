{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}
-- | A module for converting between rational types.
module MathPrelude.Classes.Rational
    ( Rational
    , CharZero(..)
    , Q(..)
    , Fractional
    , fromRational
    , fromRational98
    , toRational98
    ) where

------------------------------
--- Imports
------------------------------
import           MathPrelude.Prelude.CorePrelude
import qualified Data.Ratio                      as Ratio98
import qualified Prelude                         as P

import           MathPrelude.Classes.Integral
import           MathPrelude.Constructions.Ratio

------------------------------
--- Classes
------------------------------
-- | The canonical version of a rational number.
type Rational = Ratio Integer

-- | The only morphisms of fields are injections. Thus there is a map from the rationals to a field if and only if the field has characteristic zero (ie it contains a subfield equal to the rationals).
class CharZero a where
    -- | Convert from a 'Rational' number to an element of the field. The prime is because the function fromRational must be defined on the prelude's Rational type, and is a reserved name.
    fromRational' ∷ Rational → a

    {-# MINIMAL fromRational' #-}

-- | This class asserts that the field is not only characteristic zero, but is infact isomorphic to the rationals. Thus we can convert back and forth between the two types.
class CharZero a ⇒ Q a where
    -- | Convert back into a canonical 'Rational'
    toRational ∷ a → Rational
    -- | Convert between any two machine representations of the rationals.
    convRational ∷ Q b ⇒ a → b
    convRational = fromRational' . toRational

    {-# MINIMAL toRational #-}

-- | For backwards compatibility with the prelude's Fractional class, that roughly corresponds to the above.
class CharZero a ⇒ Fractional a

------------------------------
--- Methods
------------------------------
-- | Used to marshall numeric literals into types. Must be defined on prelude's rational type. Needed to use expresions such as 0.5 if RebindaleSyntax is enabled.
fromRational ∷ CharZero a ⇒ P.Rational → a
fromRational x = fromRational' (Ratio98.numerator x :% Ratio98.denominator x)

-- | Helper function to go between the prelude's and our rational types
toRational98 ∷ Rational → P.Rational
toRational98 x = numerator x Ratio98.% denominator x
-- | Helper function to go between the prelude's and our rational types
fromRational98 ∷ P.Rational → Rational
fromRational98 x = Ratio98.numerator x :% Ratio98.denominator x

------------------------------
--- Instances
------------------------------
instance Integral a ⇒ CharZero (Ratio a) where
     fromRational' (x :% y) = fromInteger x :% fromInteger y
instance Integral a ⇒ Q (Ratio a) where
     toRational (x :% y) = toInteger x :% toInteger y


instance CharZero Float where    fromRational' = P.fromRational . toRational98
instance CharZero Double where fromRational' = P.fromRational . toRational98

instance Q Float where toRational = fromRational98 . P.toRational
instance Q Double where toRational = fromRational98 . P.toRational

instance Fractional Float
instance Fractional Double
