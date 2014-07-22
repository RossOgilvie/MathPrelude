{-# LANGUAGE RebindableSyntax, BangPatterns, UnicodeSyntax #-}
module MathPrelude.Extras.EllipticFunctions
  (
  agm
  , ellipticF
  , ellipticE
  , ellipticPi
  , completeK
  , completeE
  , completePi
  , carlsonF
  , carlsonC
  , carlsonJ
  , carlsonD
  , jacobi_theta00
  , jacobi_theta01
  , jacobi_theta10
  , jacobi_theta11
  , sn, cn, dn
  , ns, nc, nd
  , sc, sd, cs, cd, ds, dc
  ) where

-----------------------------------
--- Imports
-----------------------------------
import BasicPrelude
import MathPrelude.Algebraic.Field
import MathPrelude.Common.Convergence
import MathPrelude.Common.Transcendental
import MathPrelude.Common.Integral
import MathPrelude.Constructions.Complex
-- import MathPrelude.Algebraic.Module

-----------------------------------
--- Carlson Symmetric Integrals
-----------------------------------
-- carl_seq :: a → a → a → a
carl_seq x y z = (\(a,_,_) → a) . converge' test . iterate step $ start
  where
    start = (x,y,z)
    test (a,b,c) _ = (a =~ b) && (b=~ c) && (a=~c)
    step (a,b,c) = (a',b',c')
      where
        a' = (a+l)/4
        b' = (b+l)/4
        c' = (c+l)/4
        l = sqrt (a*b) + sqrt (b*c) + sqrt (a*c)

carlsonF x y z = (\m -> recip . sqrt $ m) $ carl_seq x y z
carlsonC x y = carlsonF x y y
carlsonD x y z = carlsonJ x y z z
carlsonJ x y z p
  | x =~ y && x =~ z && x =~ p = x **(-3/2)
  | otherwise = (1/4) * carlsonJ x' y' z' p' + 6*carlsonC d2 s
    where
      l = sqrt (x*y) + sqrt (y*z) + sqrt (x*z)
      x' = (x + l)/4
      y' = (y + l)/4
      z' = (z + l)/4
      p' = (p + l)/4
      d = (sqrt p + sqrt x)*(sqrt p + sqrt y)*(sqrt p + sqrt z)
      d2 = d^2
      s = d2 + (p-x)*(p-y)*(p-z)

-----------------------------------
--- Incomplete Integrals
-----------------------------------

-- | The incomplete elliptic integral of the first kind. The first argument is the amplitude and the second is the modulus k.
-- <https://en.wikipedia.org/wiki/Elliptic_integral#Complete_elliptic_integral_of_the_first_kind Wikipedia>
ellipticF :: Transcendental a ⇒ a → a → a
ellipticF f k = (sin f) * carlsonF ((cos f)^2) (1-k^2*(sin f)^2) 1

-- | The incomplete elliptic integral of the second kind. The first argument is the amplitude and the second is the modulus k.
-- <https://en.wikipedia.org/wiki/Elliptic_integral#Complete_elliptic_integral_of_the_second_kind Wikipedia>
ellipticE :: Transcendental a ⇒ a → a → a
ellipticE f k = (sin f) * carlsonF ((cos f)^2) (1-k^2*(sin f)^2) 1 - (1/3)* k^2 *(sin f)^3 * carlsonD ((cos f)^2) (1-k^2*(sin f)^2) 1

-- | The incomplete elliptic integral of the third kind. The first argument is the amplitude, the second is the characteristic and the third is the modulus k.
-- <https://en.wikipedia.org/wiki/Elliptic_integral#Complete_elliptic_integral_of_the_third_kind Wikipedia>
ellipticPi :: Transcendental a ⇒ a → a → a → a
ellipticPi f n k = (sin f) * carlsonF ((cos f)^2) (1-k^2*(sin f)^2) 1 - (1/3)*n*(sin f)^3 * carlsonJ ((cos f)^2) (1-k^2*(sin f)^2) 1 (1-n*(sin f)^2)

-----------------------------------
--- Complete Integrals
-----------------------------------
-- | The arithmetic geometric mean.
-- <https://en.wikipedia.org/wiki/Arithmetic%E2%80%93geometric_mean Wikipedia>
agm :: Transcendental a => a -> a -> a
agm a g = fst . head . dropWhile test . iterate next $ (a,g)
  where
    next (!a,!g) = ((a+g)/ 2, sqrt (a*g))
    test (!a,!g) = not $ a =~ g

-- | The complete elliptic integral of the first kind. The argument is the modulus k.
-- <https://en.wikipedia.org/wiki/Elliptic_integral#Complete_elliptic_integral_of_the_first_kind Wikipedia>
completeK :: Transcendental a => a -> a
completeK k = (pi/2)/ agm (1-k) (1+k)

-- | The complete elliptic integral of the second kind. The argument is the modulus k.
-- <https://en.wikipedia.org/wiki/Elliptic_integral#Complete_elliptic_integral_of_the_second_kind Wikipedia>
completeE :: Transcendental a => a -> a
completeE k = carlsonF 0 (1-k^2) 1 - (1/3)* k^2* carlsonD 0 (1-k^2) 1

-- | The complete elliptic integral of the third kind. The second argument is the modulus k.
-- <https://en.wikipedia.org/wiki/Elliptic_integral#Complete_elliptic_integral_of_the_third_kind Wikipedia>
completePi :: Transcendental a ⇒ a → a → a
completePi n k = carlsonF 0 (1-k^2) 1 - (1/3)*n* carlsonJ 0 (1-k^2) 1 (1-n)

------------------------------------
-- Jacobi Theta
------------------------------------
jacobi_theta z tau = 1 + 2* series' term 1
  where
    term n = q^(n^2) * cos (2*pi*n'*z)
      where n' = fromIntegral n
    q = exp (pi*iu*tau)

jacobi_theta00 z tau = jacobi_theta z tau

jacobi_theta01 z tau = jacobi_theta (z + (1/2)) tau
jacobi_theta10 z tau = exp (pi*iu*tau/4 + pi*iu*z) * jacobi_theta (z + (tau/2)) tau
jacobi_theta11 z tau = exp (pi*iu*tau/4 + pi*iu*(z + (1/2))) * jacobi_theta (z + (1/2) + (tau/2)) tau

------------------------------------
-- Jacobi Elliptic
------------------------------------
sn :: (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
sn u k = - (t00* jacobi_theta11 z tau) / (t10 * jacobi_theta01 z tau)
  where
    t00 = jacobi_theta00 0 tau
    t01 = jacobi_theta01 0 tau
    t10 = jacobi_theta10 0 tau
    t11 = jacobi_theta11 0 tau
    k' = sqrt (1-k^2)
    k'' = sqrt k'
    l = (1-k'')/(1+k'')/2
    q = l + 2*l^5 + 15*l^9 + 150*l^13 + 1707*l^17 + 20910*l^21 + 268616*l^25
    tau = log q / (iu*pi)
    z = u/pi/(t00^2)

cn :: (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
cn u k = (t01* jacobi_theta10 z tau) / (t10 * jacobi_theta01 z tau)
  where
    t00 = jacobi_theta00 0 tau
    t01 = jacobi_theta01 0 tau
    t10 = jacobi_theta10 0 tau
    t11 = jacobi_theta11 0 tau
    k' = sqrt (1-k^2)
    k'' = sqrt k'
    l = (1-k'')/(1+k'')/2
    q = l + 2*l^5 + 15*l^9 + 150*l^13 + 1707*l^17 + 20910*l^21 + 268616*l^25
    tau = log q / (iu*pi)
    z = u/pi/(t00^2)

dn :: (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
dn u k = (t01* jacobi_theta00 z tau) / (t00 * jacobi_theta01 z tau)
  where
    t00 = jacobi_theta00 0 tau
    t01 = jacobi_theta01 0 tau
    t10 = jacobi_theta10 0 tau
    t11 = jacobi_theta11 0 tau
    k' = sqrt (1-k^2)
    k'' = sqrt k'
    l = (1-k'')/(1+k'')/2
    q = l + 2*l^5 + 15*l^9 + 150*l^13 + 1707*l^17 + 20910*l^21 + 268616*l^25
    tau = log q / (iu*pi)
    z = u/pi/(t00^2)

ns :: (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
ns = 1 / sn
nc :: (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
nc = 1 / cn
nd :: (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
nd = 1 / dn

sc :: (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
sc = sn / cn
sd :: (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
sd = sn / dn
ds :: (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
ds = dn / sn
dc :: (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
dc = dn / cn
cs :: (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
cs = cn / sn
cd :: (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
cd = cn / dn
------------------------------------
-- Misc
------------------------------------
-- aperiod :: Double -> Complex Double
-- -- aperiod :: (Ord a, Field a, Transcendental a) => a -> Complex a Double -> Complex Double
-- aperiod r = (-4::Double) .* iu * (fromReal $ ellipticK (r ^ 2))
-- --bperiod :: (Ord a, Field a, Transcendental a) => a -> Complex a
-- bperiod :: Double -> Complex Double
-- bperiod r = ((-8)/(1+r)/(1+r)) .* (fromReal $ ellipticK ktilde)
--   where ktilde = (1-r)*(1-r)/(1+r)/(1+r)
--
-- tau r = (aperiod r)/(bperiod r)
