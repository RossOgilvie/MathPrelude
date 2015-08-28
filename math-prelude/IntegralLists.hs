{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | The 'mathless-prelude' has some list functions that take Ints. This module generalises them to take any integral type.
-- | This module did do that, but then it fails without explicit type annotations. For now, use Integer
module IntegralLists
  ( module BasicPrelude
  , length
  , take
  , drop
  , splitAt
  , (!!)
  , replicate
  , length'
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

import BasicPrelude hiding
  ( length
  , take
  , drop
  , splitAt
  , (!!)
  , replicate
  )

import qualified BasicPrelude as B

import MathPrelude.Classes.Integral

toInt ∷ Integral a ⇒ a → Int
toInt = fromIntegral

fromInt ∷ Integral a ⇒ Int → a
fromInt = fromIntegral

------------------------------------
--- Integer Lists
------------------------------------

-- | The length of a list
length ∷  [a] → Integer
length = fromInt . B.length
-- | Take at most this number of elements from the given list
take ∷  Integer → [b] → [b]
take n = B.take (toInt n)
-- | Drop (ie discard) at least this number of elements from the given list
drop ∷  Integer → [b] → [b]
drop n = B.drop (toInt n)
-- | Return take paired with the remainder of the list (which may be empty)
splitAt∷  Integer → [b] → ([b],[b])
splitAt n = B.splitAt (toInt n)
-- | Retrieve the n-th item of the list. Zero indexed. Unsafe.
(!!) ∷  [a] → Integer → a
(!!) xs n = (B.!!) xs (toInt n)
-- | Create a list consisting of the n copies of the given element.
replicate ∷  Integer → b → [b]
replicate n = B.replicate (toInt n)


------------------------------------
--- Int Lists
------------------------------------
-- | The length of a list
length' ∷  [a] → Int
length' = B.length
-- | Take at most this number of elements from the given list
take' ∷  Int → [b] → [b]
take' = B.take
-- | Drop (ie discard) at least this number of elements from the given list
drop' ∷  Int → [b] → [b]
drop' = B.drop
-- | Return take paired with the remainder of the list (which may be empty)
splitAt' ∷  Int → [b] → ([b],[b])
splitAt' = B.splitAt
-- | Create a list consisting of the n copies of the given element.
replicate' ∷  Int → b → [b]
replicate' = B.replicate



------------------------------------
--- Integral Lists
------------------------------------
-- | The length of a list
length'' ∷ Integral b ⇒ [a] → b
length'' = fromInt . B.length
-- | Take at most this number of elements from the given list
take'' ∷ Integral a ⇒ a → [b] → [b]
take'' n = B.take (toInt n)
-- | Drop (ie discard) at least this number of elements from the given list
drop'' ∷ Integral a ⇒ a → [b] → [b]
drop'' n = B.drop (toInt n)
-- | Return take paired with the remainder of the list (which may be empty)
splitAt'' ∷ Integral a ⇒ a → [b] → ([b],[b])
splitAt'' n = B.splitAt (toInt n)
-- | Create a list consisting of the n copies of the given element.
replicate'' ∷ Integral a ⇒ a → b → [b]
replicate'' n = B.replicate (toInt n)
