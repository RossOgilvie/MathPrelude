{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds   #-}

module MathPrelude.Extras.NumberRings
  ( Z(..)
  , modulusZ
  ) where

import MathPrelude
import qualified Prelude                          as P

-- From math-prelude
import           MathPrelude.Classes.Field

-- From math-extras
import           MathPrelude.Extras.NumberRingsTH

-- TH stuff
import           Data.Proxy
import           GHC.TypeLits
import           Language.Haskell.TH



instance KnownNat n ⇒ Show (Z n) where
  show (Z a) = P.show a

instance KnownNat n ⇒ Monoid (Z n) where
  mappend = liftZ2 mappend
  mempty = Z 0

instance KnownNat n ⇒ Approx (Z n) where
  (=~) = liftZ2' (=~)

instance KnownNat n ⇒ Group (Z n) where
  negate = liftZ negate

instance KnownNat n ⇒ Abelian (Z n)

instance KnownNat n ⇒ Ring (Z n) where
  (*) = liftZ2 (*)
  one = Z 1

instance KnownNat n ⇒ CRing (Z n)

$(mkFiniteFields primes)
