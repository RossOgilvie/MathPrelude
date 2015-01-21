{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}

-- | The 'mathless-prelude' has some list functions that take Ints. This module generalises them to take any integral type.
-- | This module did do that, but then it fails without explicit type annotations. For now, use Integer
module MathPrelude.Exports.IntegralLists
  ( module BasicPrelude
  , length
  , take
  , drop
  , splitAt
  , (!!)
  , replicate
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

-- -- | The length of a list
-- length ∷ Integral b ⇒ [a] → b
-- length = fromInt . B.length
-- -- | Take at most this number of elements from the given list
-- take ∷ Integral a ⇒ a → [b] → [b]
-- take n = B.take (toInt n)
-- -- | Drop (ie discard) at least this number of elements from the given list
-- drop ∷ Integral a ⇒ a → [b] → [b]
-- drop n = B.drop (toInt n)
-- -- | Return take paired with the remainder of the list (which may be empty)
-- splitAt∷ Integral a ⇒ a → [b] → ([b],[b])
-- splitAt n = B.splitAt (toInt n)
-- -- | Retrieve the n-th item of the list. Zero indexed. Unsafe.
-- (!!) ∷ Integral b ⇒ [a] → b → a
-- (!!) xs n = (B.!!) xs (toInt n)
-- -- | Create a list consisting of the n copies of the given element.
-- replicate ∷ Integral a ⇒ a → b → [b]
-- replicate n = B.replicate (toInt n)

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
