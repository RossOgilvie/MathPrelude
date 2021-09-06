{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE ImplicitPrelude #-}

module MathPrelude.Prelude.NamedNumbers
    ( 
    Int32
    , Int64
    , Word
    , Word32
    , Word64
    
    -- , zeroInteger
    -- , zeroInt
    -- , zeroInt32
    -- , zeroInt64
    -- , zeroWord
    -- , zeroWord32
    -- , zeroWord64
    -- , zeroFloat
    -- , zeroDouble
    , zeroGeneric

    -- , oneInteger
    -- , oneInt
    -- , oneInt32
    -- , oneInt64
    -- , oneWord
    -- , oneWord32
    -- , oneWord64
    -- , oneFloat
    -- , oneDouble
    , oneGeneric

    , twoInteger
    , epsFloat
    , epsDouble
    ) where

-- import Prelude(fromInteger, fromRationalInt, Integer, Float, Double)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)

-- * Constants
-- $desc

-- $desc
-- When using the rebindable syntax extension, one no longer has access to the prelude's versions of fromInteger and fromRational, so it is initially impossible to use numeric literals. Instead, they are defined here and exported by name for later use.

-- zeroInteger :: Integer
-- zeroInteger = 0 ∷ Integer
-- zeroInt :: Int
-- zeroInt = 0 ∷ Int
-- zeroInt32 :: Int32
-- zeroInt32 = 0 ∷ Int32
-- zeroInt64 :: Int64
-- zeroInt64 = 0 ∷ Int64
-- zeroWord :: Word
-- zeroWord = 0 ∷ Word
-- zeroWord32 :: Word32
-- zeroWord32 = 0 ∷ Word32
-- zeroWord64 :: Word64
-- zeroWord64 = 0 ∷ Word64
-- zeroFloat :: Float
-- zeroFloat = 0 ∷ Float
-- zeroDouble :: Double
-- zeroDouble = 0 ∷ Double
zeroGeneric :: Num a ⇒ a
zeroGeneric = 0

-- oneInteger :: Integer
-- oneInteger = 1 ∷ Integer
-- oneInt :: Int
-- oneInt = 1 ∷ Int
-- oneInt32 :: Int32
-- oneInt32 = 1 ∷ Int32
-- oneInt64 :: Int64
-- oneInt64 = 1 ∷ Int64
-- oneWord :: Word
-- oneWord = 1 ∷ Word
-- oneWord32 :: Word32
-- oneWord32 = 1 ∷ Word32
-- oneWord64 :: Word64
-- oneWord64 = 1 ∷ Word64
-- oneFloat :: Float
-- oneFloat = 1 ∷ Float
-- oneDouble :: Double
-- oneDouble = 1 ∷ Double
oneGeneric :: Num a ⇒ a
oneGeneric = 1

twoInteger :: Integer
twoInteger = 2 ∷ Integer
epsFloat :: Float
epsFloat = 1e-5 ∷ Float
epsDouble :: Double
epsDouble = 1e-10 ∷ Double