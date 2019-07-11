-- {-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}

module MathPrelude.Prelude.NamedNumbers
    ( 
    Int32
    , Int64
    , Word
    , Word32
    , Word64
    
    , zeroInteger
    , zeroInt
    , zeroInt32
    , zeroInt64
    , zeroWord
    , zeroWord32
    , zeroWord64
    , zeroFloat
    , zeroDouble

    , oneInteger
    , oneInt
    , oneInt32
    , oneInt64
    , oneWord
    , oneWord32
    , oneWord64
    , oneFloat
    , oneDouble

    , twoInteger
    , epsFloat
    , epsDouble
    ) where

-- import Prelude(fromInteger, fromRationalInt, Integer, Float, Double)
import Data.Int (Int32, Int64)
import Data.Word (Word, Word32, Word64)

-- * Constants
-- $desc

-- $desc
-- When using the rebindable syntax extension, one no longer has access to the prelude's versions of fromInteger and fromRational, so it is initially impossible to use numeric literals. Instead, they are defined here and exported by name for later use.

zeroInteger = 0 ∷ Integer
zeroInt = 0 ∷ Int
zeroInt32 = 0 ∷ Int32
zeroInt64 = 0 ∷ Int64
zeroWord = 0 ∷ Word
zeroWord32 = 0 ∷ Word32
zeroWord64 = 0 ∷ Word64
zeroFloat = 0 ∷ Float
zeroDouble = 0 ∷ Double

oneInteger = 1 ∷ Integer
oneInt = 1 ∷ Int
oneInt32 = 1 ∷ Int32
oneInt64 = 1 ∷ Int64
oneWord = 1 ∷ Word
oneWord32 = 1 ∷ Word32
oneWord64 = 1 ∷ Word64
oneFloat = 1 ∷ Float
oneDouble = 1 ∷ Double

twoInteger = 2 ∷ Integer

epsFloat = 1e-5 ∷ Float
epsDouble = 1e-10 ∷ Double
