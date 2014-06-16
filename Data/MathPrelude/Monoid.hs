{-# LANGUAGE NoImplicitPrelude #-}
module Data.MathPrelude.Monoid where

import BasicPrelude
import qualified Prelude as P

import Data.MathPrelude.PreludeNumConst

instance Monoid Int where mempty = zeroInt; mappend = (P.+)
instance Monoid Integer where mempty = zeroInteger; mappend = (P.+)
instance Monoid Int32 where mempty = zeroInt32; mappend = (P.+)
instance Monoid Int64 where mempty = zeroInt64; mappend = (P.+)
instance Monoid Float where mempty = zeroFloat; mappend = (P.+)
instance Monoid Double where mempty = zeroDouble; mappend = (P.+)

sum :: Monoid a => [a] -> a
sum = foldr mappend mempty
