{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE ImplicitPrelude #-}

-- | A module with the basic logical operators
module MathPrelude.Prelude.Logic
    ( Prelude.not
    , (Prelude.&&)
    , (Prelude.||)
    , Prelude.otherwise
    , ifThenElse
    , L.and, L.or
    , implies
    , xor
    , iff
    , neither
    ) where

-- import           MathPrelude.Prelude.CorePrelude
import qualified Data.List    as L
-- import qualified Prelude      as P

-- | This binary logic function corresponds to the formal logic implication operator, aka material implication or entailment. It is false if and only if the first argument is true and the second is false.
implies ∷ Bool → Bool → Bool
implies True True = True
implies True False = False
implies False True = True
implies False False = True

-- | The exclusive or. As in, "Do you want $10,000 or a holiday to Peru", compared to "Would you like milk or sugar?". True if just one argument is true.
xor ∷ Bool → Bool → Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

-- | The exclusive or. As in, "Do you want $10,000 or a holiday to Peru", compared to "Would you like milk or sugar?". True if just one argument is true.
iff ∷ Bool → Bool → Bool
iff True True = True
iff True False = False
iff False True = False
iff False False = True

-- | Not Or. Is true exactly when both arguments are false.
neither ∷ Bool → Bool → Bool
neither x y = not $ (&&) x y

-- | Exactly what you expect. This is required since the rebindable syntax extension overrides this as well, and without it "if-then-else" syntax won't work.
ifThenElse ∷ Bool → a → a → a
ifThenElse t a b
    | t = a
    | otherwise = b