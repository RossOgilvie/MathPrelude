{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
-- | A module with the basic logical operators
module MathPrelude.Algebraic.Logic
	( P.not
	, (P.&&), (P.||)
	, L.and, L.or
	, implies, xor, iff, neither
	, ifThenElse
	) where

import BasicPrelude
import qualified Prelude as P
import qualified Data.List as L

-- | This binary logic function corresponds to the formal logic implication operator, aka logical consequence or entailment. It is false if and only if the first argument is true and the second is  false.
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
neither x y = P.not $ (P.&&) x y

-- | Exactly what you expect. This is required since the rebindable syntax extension overrides this as well, and without it "if-then-else" syntax won't work.
ifThenElse ∷ Bool → a → a → a
ifThenElse t a b
	| t = a
	| otherwise = b
