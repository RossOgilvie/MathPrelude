{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
module MathPrelude.Constructions.ContinuedFractions where

----------------------------------
-- Imports
----------------------------------
import BasicPrelude
import MathPrelude.Algebraic.EuclideanDomain
import MathPrelude.Algebraic.Field
import MathPrelude.Constructions.Ratio
import MathPrelude.Common.Integral
import MathPrelude.Common.Rational
import MathPrelude.Common.Transcendental
import MathPrelude.Classes.Derivation

-- | A continued fraction
data CF = CF [Integer] deriving Show

data TR = TR Integer Integer Integer Integer Integer Integer Integer Integer deriving Show

agreement ∷ TR → (Bool , Maybe Integer)
agreement (TR a b c d e f g h) = (agree, a1)
  where
    mDiv x y
      | y == 0 = Nothing
      | otherwise = Just (div x y)
    a1 = mDiv a e
    a2 = mDiv b f
    a3 = mDiv c g
    a4 = mDiv d h
    agree = a1 == a2 && a2 == a3 && a3 == a4

output ∷ Integer → TR → TR
output r (TR a b c d e f g h) = TR e f g h (a-e*r) (b-f*r) (c-g*r) (d-h*r)

inputx ∷ Integer → TR → TR
inputx p (TR a b c d e f g h) = TR b (a+b*p) d (c+d*p) f (e+f*p) h (g+h*p)

inputx' ∷ TR → TR
inputx' (TR a b c d e f g h) = TR b b d d f f h h

inputy ∷ Integer → TR → TR
inputy q (TR a b c d e f g h) = TR c d (a+c*q) (b+d*q) g h (e+g*q) (f+h*q)

inputy' ∷ TR → TR
inputy' (TR a b c d e f g h) = TR c d c d g h g h

choose_x ∷ TR → Bool
choose_x (TR a b c d e f g h) = abs ((b*e - a*f)*(g*e)) > abs ((c*e - a*g)*(f*e))

endgameTR ∷ TR → TR
endgameTR (TR a b c d e f g h) = TR d d d d h h h h

doTR ∷ TR → CF → CF -> CF
doTR tr = liftCF2 (doTR' tr)
doTR' ∷ TR → [Integer] → [Integer] -> [Integer]
doTR' tr@(TR a b c d e f g h) xs ys
  | agbool = if isJust ag then ag': doTR' (output ag' tr) xs ys else []
  | null xs && null ys = doTR' (endgameTR tr) xs ys
  | null xs = doTR' (inputy y tr) xs ys'
  | null ys = doTR' (inputx x tr) xs' ys
  | choose_x tr = doTR' (inputx x tr) xs' ys
  | otherwise = doTR' (inputy y tr) xs ys'
  where
    (agbool, ag) = agreement tr
    Just ag' = ag
    (x:xs') = xs
    (y:ys') = ys

trAdd = TR 0 1 1 0 1 0 0 0
trSub = TR 0 1 (-1) 0 1 0 0 0
trNeg = TR 0 (-1) 0 0 1 0 0 0
trMul = TR 0 0 0 1 1 0 0 0
trDiv = TR 0 1 0 0 0 0 1 0
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

-- | Evaluate a continued fraction to a field element
evalCF ∷ Field a ⇒ CF → a
evalCF (CF ls)= evalCF_new ls

evalCF_old ∷ Field a ⇒ [Integer] → a
evalCF_old [x] = fromInteger x
evalCF_old (x:xs) = fromInteger x + 1 / evalCF_old xs

evalCF_new as = evaluateR $ map fromInteger result
  where
    nums = 0 : 1 : zipWith (+) nums (zipWith (*) as nums' )
    nums' = drop 1 nums
    dens = 1 : 0 : zipWith (+) dens (zipWith (*) as dens' )
    dens' = drop 1 dens
    lows = even_terms $ zipWith (:%) (drop 2 nums) (drop 2 dens)
    highs = odd_terms $ zipWith (:%) (drop 2 nums) (drop 2 dens)
    test ((a:%b), (c:%d)) = ieps * (a*d - b*c) > b*d
    converge = dropWhile test $ zip highs lows
    result = if null converge then last nums :% last dens else fst . head $ converge

even_terms (x:y:xs) = x : even_terms xs
even_terms [x] = [x]
even_terms [] = []
odd_terms (x:y:xs) = y : odd_terms xs
odd_terms [x] = []
odd_terms [] = []

e = CF $ 2 : 1 : concatMap (\k → [2*k,1,1]) [1..]
sqrt2 = CF $ 1 : repeat 2

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

instance NumEq (CF) where
  (=~) (CF ls) (CF ls') = ls == ls'
  epsilon = recip $ makeCF $ fromInteger (10^5)
  nearZero cf = cf < epsilon
  (>>~) cf1 cf2 = cf2 / cf1 < epsilon

sigfigs = 16
ieps = 10^sigfigs
eps = recip $ makeCF $ fromInteger ieps

instance Monoid (CF) where
  mempty = CF [zero]
  mappend = doTR trAdd

instance Group (CF) where
  negate p = doTR trNeg p 0
  (-) = doTR trSub
instance Abelian (CF) where

instance Ring (CF) where
  one = CF [one]
  (*) = doTR trMul

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
