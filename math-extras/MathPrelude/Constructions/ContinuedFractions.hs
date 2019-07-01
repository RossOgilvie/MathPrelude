{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}

-- | A continued fraction can be seen as the generalisation of the rational numbers. Infinite continued fractions can be used to concisely compute irrational numbers. In this sense, they can be used as computable reals. Implementation here is kinda broken.
module MathPrelude.Constructions.ContinuedFractions where

----------------------------------
-- Imports
----------------------------------
import MathPrelude
import qualified Data.Maybe                          as M
import           MathPrelude.Calculus.Derivation
import           MathPrelude.Classes.EuclideanDomain
import           MathPrelude.Classes.Field
import           MathPrelude.Classes.Integral
import           MathPrelude.Classes.Rational
import           MathPrelude.Classes.Transcendental
import           MathPrelude.Constructions.Ratio

import           Test.QuickCheck                     hiding (output)

-- | A continued fraction
data CF = CF [Integer] deriving Show


-------------------------------------------
-- The crazy do everything method
-------------------------------------------
data TR = TR Integer Integer Integer Integer Integer Integer Integer Integer deriving Show

agreement ∷ TR → (Bool , Bool, Integer)
agreement (TR a b c d e f g h) = (agree, divByZero, M.fromJust a1)
  where
    mDiv x y
      | y == 0 = Nothing
      | otherwise = Just (div x y)
    a1 = mDiv a e
    a2 = mDiv b f
    a3 = mDiv c g
    a4 = mDiv d h
    divByZero = h == 0
    agree = a1 == a2 && a2 == a3 && a3 == a4

output ∷ Integer → TR → TR
output r (TR a b c d e f g h) = TR e f g h (a-e*r) (b-f*r) (c-g*r) (d-h*r)

inputx ∷ Integer → TR → TR
inputx p (TR a b c d e f g h) = TR b (a+b*p) d (c+d*p) f (e+f*p) h (g+h*p)

inputy ∷ Integer → TR → TR
inputy q (TR a b c d e f g h) = TR c d (a+c*q) (b+d*q) g h (e+g*q) (f+h*q)

inputx' ∷ TR → [Integer] → [Integer]
inputx' (TR a b c d e f g h) ys
  | agree = if divByZero then [] else value : inputx' (output value tr') ys
  | null ys = endgameTR tr'
  | otherwise = inputx' (inputy (head ys) tr') (tail ys)
  where
    tr' = TR b b d d f f h h
    (agree, divByZero, value) = agreement tr'

inputy' ∷ TR → [Integer] -> [Integer]
inputy' (TR a b c d e f g h) xs
  | agree = if divByZero then [] else value : inputy' (output value tr') xs
  | null xs = endgameTR tr'
  | otherwise = inputy' (inputx (head xs) tr') (tail xs)
  where
    tr' = TR c d c d g h g h
    (agree, divByZero, value) = agreement tr'

endgameTR ∷ TR → [Integer]
endgameTR tr@(TR a b c d e f g h)
  | h == 0 = []
  | otherwise = let r = div d h in r : endgameTR (output r tr)

choose_x ∷ TR → Bool
choose_x (TR a b c d e f g h) = abs ((b*e - a*f)*g) > abs ((c*e - a*g)*f)

doTR ∷ TR → CF → CF -> CF
doTR tr = liftCF2 (doTR' tr)
doTR' ∷ TR → [Integer] → [Integer] -> [Integer]
doTR' !tr [] [] = endgameTR tr
doTR' !tr xs ys
  | agree = if divByZero then [] else value : doTR' (output value tr) xs ys
  | choose_x tr = if null xs then inputx' tr ys else doTR' (inputx (head xs) tr) (tail xs) ys
  | otherwise = if null ys then inputy' tr xs else doTR' (inputy (head ys) tr) xs (tail ys)
  where
    (agree, divByZero, value) = agreement tr

trAdd = TR 0 1 1 0 1 0 0 0
trSub = TR 0 1 (-1) 0 1 0 0 0
trNeg = TR 0 (-1) 0 0 1 0 0 0
trMul = TR 0 0 0 1 1 0 0 0
trDiv = TR 0 1 0 0 0 0 1 0

-------------------------------------------
-- Sanity Check
-------------------------------------------
instance (Arbitrary a, Approx a, Monoid a) => Arbitrary (Ratio a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary `suchThat` (/=~ mempty)
    return (x:%y)
testAdd ∷ Rational → Rational → Bool
testAdd x y = (makeCF x) + (makeCF y) == makeCF (x+y)

-------------------------------------------
-- Helpers
-------------------------------------------

-- Construct a continued fraction from a 'Rational' number
makeCF ∷ Rational → CF
makeCF q = CF $ makeCF' (numerator q') (denominator q')
  where q' = simplifyR q
makeCF' a b
  | b =~ 0 = []
  | b =~ 1 = [a]
  | otherwise = d : makeCF' b m
  where (d,m) = divMod a b

liftCF f (CF l) = CF $ f l
liftCF2 f (CF l) (CF l') = CF $ f l l'
liftCF2' f (CF l) (CF l') = f l l'

-- | Take the whole number part of a continued fraction
leadingCF (CF (x:_)) = x

toList (CF ls) = ls

-------------------------------------------
-- Evaling
-------------------------------------------

-- | Evaluate a continued fraction to a field element
evalCF ∷ Field a ⇒ CF → a
evalCF (CF ls) = evalCF_new ls

evalCF_old ∷ Field a ⇒ [Integer] → a
evalCF_old [x] = fromInteger x
evalCF_old (x:xs) = fromInteger x + 1 / evalCF_old xs

evalCF_new as = evaluateR $ map fromInteger result
  where
    test (a:%b, c:%d) = ieps * (a*d - b*c) > b*d
    (lows,highs) = boundsCF as
    converge = dropWhile test $ zip lows highs
    exact = if length lows == length highs then last highs else last lows
    result = if null converge then exact else fst . head $ converge

boundsCF as = (lows, highs)
  where
    nums = 0 : 1 : zipWith (+) nums (zipWith (*) as nums' )
    nums' = drop 1 nums
    dens = 1 : 0 : zipWith (+) dens (zipWith (*) as dens' )
    dens' = drop 1 dens
    lows = even_terms $ zipWith (:%) (drop 2 nums) (drop 2 dens)
    highs = odd_terms $ zipWith (:%) (drop 2 nums) (drop 2 dens)

even_terms (x:y:xs) = x : even_terms xs
even_terms [x] = [x]
even_terms [] = []
odd_terms (x:y:xs) = y : odd_terms xs
odd_terms [x] = []
odd_terms [] = []


-------------------------------------------
-- Constants
-------------------------------------------
-- | Euler's number, ie 2.718281828...
euler ∷ CF
euler = CF $ 2 : 1 : concatMap (\k → [2*k,1,1]) [1..]
-- | The square root of two.
sqrt2 ∷ CF
sqrt2 = CF $ 1 : repeat 2


-------------------------------------------
-- Instances
-------------------------------------------
instance Eq CF where
  (==) (CF a) (CF b) = a == b

instance Ord CF where
  compare = liftCF2' compare'
    where
      compare' [] [] = EQ
      compare' [] _ = GT
      compare' _ [] = LT
      compare' (x:xs) (y:ys)
        | compare x y == EQ = opp $ compare' xs ys
        | otherwise = compare x y
      opp EQ = EQ
      opp LT = GT
      opp GT = LT

instance Approx (CF) where
  (=~) (CF ls) (CF ls') = ls == ls'
  -- epsilon = recip $ makeCF $ fromInteger (10^5)
  -- nearZero cf = cf < epsilon
  -- (>>~) cf1 cf2 = cf2 / cf1 < epsilon

sigfigs = 16
ieps = 10^sigfigs
eps = recip $ makeCF $ fromInteger ieps

instance Semigroup (CF) where
  (<>) = doTR trAdd

instance Monoid (CF) where
  mempty = CF [zero]

instance Group (CF) where
  negate p = doTR trNeg p 0
  (-) = doTR trSub
instance Abelian (CF) where

instance Ring (CF) where
  one = CF [one]
  (*) = doTR trMul

instance CRing (CF)

instance IntDom (CF)

instance Field (CF) where
  recip = liftCF recip'
  (/) = doTR trDiv

recip' (x:xs)
  | x =~ 0 = xs
  | otherwise = 0:x:xs

instance CharZero CF where
  fromRational' = makeCF

instance Derivation CF where
  derive _ = zero
--
-- instance Q CF where
--   toRational = evalCF

-- instance Transcendental CF where
--   pi =
