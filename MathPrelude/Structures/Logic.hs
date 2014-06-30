{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Structures.Logic
	( implies, xor
	) where

import BasicPrelude
import qualified Prelude as P

implies :: Bool -> Bool -> Bool
implies True True = True
implies True False = False
implies False True = True
implies False False = True

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False
