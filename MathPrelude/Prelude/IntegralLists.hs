{-# LANGUAGE UnicodeSyntax #-}

-- | Often we want to use list functions with integral types other than Int, most often Integer. We supply alternative list functions, named with a prime eg length', that use Integer. List function working with an arbitrary Integral type are available with a double prime, eg length''
module MathPrelude.Prelude.IntegralLists
  ( length'
  , take'
  , drop'
  , splitAt'
  , replicate'
  , length''
  , take''
  , drop''
  , splitAt''
  , replicate''
  ) where

import qualified MathPrelude.Classes.Integral as MP

toInt ∷ MP.Integral a ⇒ a → Int
toInt = MP.fromIntegral

fromInt ∷ MP.Integral a ⇒ Int → a
fromInt = MP.fromIntegral

------------------------------------
--- Integral Lists
------------------------------------

-- | The length of a list
length'' ∷ MP.Integral b ⇒ [a] → b
length'' = fromInt . length
-- | Take at most this number of elements from the given list
take'' ∷ MP.Integral a ⇒ a → [b] → [b]
take'' n = take (toInt n)
-- | Drop (ie discard) at least this number of elements from the given list
drop'' ∷ MP.Integral a ⇒ a → [b] → [b]
drop'' n = drop (toInt n)
-- | Return take paired with the remainder of the list (which may be empty)
splitAt'' ∷ MP.Integral a ⇒ a → [b] → ([b],[b])
splitAt'' n = splitAt (toInt n)
-- | Create a list consisting of the n copies of the given element.
replicate'' ∷ MP.Integral a ⇒ a → b → [b]
replicate'' n = replicate (toInt n)


------------------------------------
--- Integer Lists
------------------------------------

-- | The length of a list
length' ∷  [a] → Integer
length' = length''
-- | Take at most this number of elements from the given list
take' ∷  Integer → [b] → [b]
take' = take''
-- | Drop (ie discard) at least this number of elements from the given list
drop' ∷  Integer → [b] → [b]
drop' = drop''
-- | Return take paired with the remainder of the list (which may be empty)
splitAt'∷  Integer → [b] → ([b],[b])
splitAt' = splitAt''
-- | Create a list consisting of the n copies of the given element.
replicate' ∷  Integer → b → [b]
replicate' = replicate''



