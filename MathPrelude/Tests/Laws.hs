{-# LANGUAGE RebindableSyntax #-}
{- |
Define common properties that can be used e.g. for automated tests.
Cf. to "Test.QuickCheck.Utils".
-}
module MathPrelude.Tests.Laws where

{-
From the numeric-prelude package
Copyright (c) Dylan Thurston <dpt@math.harvard.edu>, Henning Thielemann <numericprelude@henning-thielemann.de>, Mikael Johansson
Fri Mar 14 14:08:28 UTC 2014
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}


import BasicPrelude
import MathPrelude.Common.NumEq
-- import Test.QuickCheck

commutative :: NumEq a => (b -> b -> a) -> b -> b -> Bool
commutative op x y  =  x `op` y =~ y `op` x

associative :: NumEq a => (a -> a -> a) -> a -> a -> a -> Bool
associative op x y z  =  (x `op` y) `op` z =~ x `op` (y `op` z)

leftIdentity :: NumEq a => (b -> a -> a) -> b -> a -> Bool
leftIdentity op y x  =  y `op` x =~ x

rightIdentity :: NumEq a => (a -> b -> a) -> b -> a -> Bool
rightIdentity op y x  =  x `op` y =~ x

identity :: NumEq a => (a -> a -> a) -> a -> a -> Bool
identity op x y  =  leftIdentity op x y &&  rightIdentity op x y

leftZero :: NumEq a => (a -> a -> a) -> a -> a -> Bool
leftZero  =  flip . rightIdentity

rightZero :: NumEq a => (a -> a -> a) -> a -> a -> Bool
rightZero  =  flip . leftIdentity

bothZero :: NumEq a => (a -> a -> a) -> a -> a -> Bool
bothZero op x y  =  leftZero op x y  &&  rightZero op x y

leftInverse :: NumEq a => (b -> b -> a) -> (b -> b) -> a -> b -> Bool
leftInverse op inv y x  =  inv x `op` x =~ y

rightInverse :: NumEq a => (b -> b -> a) -> (b -> b) -> a -> b -> Bool
rightInverse op inv y x  =  x `op` inv x =~ y

inverse :: NumEq a => (b -> b -> a) -> (b -> b) -> a -> b -> Bool
inverse op inv y x  =  leftInverse op inv y x && rightInverse op inv y x

leftDistributive :: NumEq a => (a -> b -> a) -> (a -> a -> a) -> b -> a -> a -> Bool
leftDistributive ( # ) op x y z  =  (y `op` z) # x =~ (y # x) `op` (z # x)

rightDistributive :: NumEq a => (b -> a -> a) -> (a -> a -> a) -> b -> a -> a -> Bool
rightDistributive ( # ) op x y z  =  x # (y `op` z) =~ (x # y) `op` (x # z)

homomorphism :: NumEq a => (b -> a) -> (b -> b -> b) -> (a -> a -> a) -> b -> b -> Bool
homomorphism f op0 op1 x y  =  f (x `op0` y) =~ f x `op1` f y

-- rightCascade :: (NumEq a, NumEq b) => (b -> b -> b) -> (a -> b -> a) -> a -> b -> b -> Bool
-- rightCascade ( # ) op x i j  =  (x `op` i) `op` j =~ x `op` i#j
--
-- leftCascade :: (NumEq a, NumEq b) => (b -> b -> b) -> (b -> a -> a) -> a -> b -> b -> Bool
-- leftCascade ( # ) op x i j  =  j `op` (i `op` x) =~ j#i `op` x
