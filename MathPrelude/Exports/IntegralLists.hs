{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}

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

length ∷ Integral b ⇒ [a] → b
length = fromInt . B.length
take ∷ Integral a ⇒ a → [b] → [b]
take n = B.take (toInt n)
drop ∷ Integral a ⇒ a → [b] → [b]
drop n = B.drop (toInt n)
splitAt∷ Integral a ⇒ a → [b] → ([b],[b])
splitAt n = B.splitAt (toInt n)
(!!) ∷ Integral b ⇒ [a] → b → a
(!!) xs n = (B.!!) xs (toInt n)
replicate ∷ Integral a ⇒ a → b → [b]
replicate n = B.replicate (toInt n)
