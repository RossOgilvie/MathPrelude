{-# LANGUAGE NoImplicitPrelude #-}
module Data.MathPrelude.Monoid where

import BasicPrelude
import qualified Prelude as P

instance Monoid Int where mempty = 0; mappend = (P.+)
instance Monoid Integer where mempty = 0; mappend = (P.+)
instance Monoid Int32 where mempty = 0; mappend = (P.+)
instance Monoid Int64 where mempty = 0; mappend = (P.+)
instance Monoid Float where mempty = 0; mappend = (P.+)
instance Monoid Double where mempty = 0; mappend = (P.+)

sum :: Monoid a => [a] -> a
sum = foldr mappend mempty
