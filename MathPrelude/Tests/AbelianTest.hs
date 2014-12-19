{-# LANGUAGE RebindableSyntax, UnicodeSyntax, ScopedTypeVariables #-}
module MathPrelude.Tests.AbelianTest where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Classes.Group
import MathPrelude.Tests.Laws
import Test.QuickCheck


fixtype ∷ a → (a→b) → (a→b)
fixtype x f = f

isGroup ∷ (Show a, Arbitrary a, Group a) ⇒ a → a → a → Bool
isGroup a b c =
  associative (<>) a b c
  && identity (<>) zero a
  && inverse (<>) negate zero a

isAbelian ∷ (Show a, Arbitrary a, Abelian a) ⇒ a → a → a → Bool
isAbelian a b c =
  isGroup a b c
  && commutative (+) a b
