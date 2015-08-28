{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE DataKinds #-}
module ModuliSpace
    ( main
    ) where

import MathPrelude
import MathPrelude.Classes.Module
import MathPrelude.Classes.Norm
import MathPrelude.Constructions.Complex
import MathPrelude.SpecialFunctions.EllipticFunctions

import Differentials

import Graphics.EasyPlot
import qualified Filesystem.Path.CurrentOS as FP
import qualified Filesystem as F

import qualified Data.Set as Set


-- main = mainPreCompute
main = mainRefine




-- known good paramters
-- m,n,mt,nt,k,kt :: Double
-- (m,n,mt,nt,k,kt) = (2,-1,0,1,0,-2)
-- symcond' = symcond m n mt nt k kt

stdOptions :: [Option3D Double Double Double]
stdOptions = [RangeX (-1) 1, RangeY (-1) 1]


numberOfPts = 30


nearInteger x eps = d' < eps || d_ < eps
    where
        x' = fromInteger $ ceiling x
        d' = x' - x -- x'>x so this is always pos
        x_ = fromInteger $ floor x
        d_ = x - x_  -- x_<x so this is always pos too

gammaMinusInt m n gn1 eps pt = nearInteger gamma_minus' eps
    where gamma_minus' = gamma_minus m n pt




--
-- symCondInt eps pt = gammaMinusInt m n eps pt && gammaMinusInt mt nt eps pt
--
-- moduliPtsGrid = filter (symCondInt 0.2) ddPts


----------------------------
-- New Sym conditions
----------------------------


-- Just dump all the calculations to files for ease of not having to redo later

-- theta^S only need the negative one, the positive has been normalised to 2πi
thetaSMinus = gamma_minus 1 0
theta2Plus = gamma_plus 0 1
theta2Minus = gamma_minus 0 1
values = map computeShit $ ddPts numberOfPts

mainPreCompute = writeOut "integral_values15/values" values

computeShit pt = (pt, thetaSMinus pt, theta2Plus pt, theta2Minus pt)


inRange l u x = l < x && x < u


manualSymCond' ∷ DD → Double
manualSymCond' (DD α β) = (norm (1+α) * norm (1+β)) / (norm (1-α) * norm (1-β))
manualSymCond ∷ Double → Double → DD → Bool
manualSymCond val eps = inRange (val-eps) (val+eps) . manualSymCond'

b0pts ∷ (Integer, [DD])
b0pts = filterAndCount (manualSymCond 2 0.05) $ ddPts numberOfPts

refinementStep num width = concatMap (makeDDGrid num width)

filterStep ∷ Double → Double → [DD] → (Integer, [DD])
filterStep val eps = filterAndCount (manualSymCond val eps)

filterStepP1 :: Double → Double → [DD] → [DD]
filterStepP1 val eps = filter (inRange (val-eps) (val+eps) . gammaPm)
    where
        gammaPm (computeShit → (pt, gammaSm, gamma2p, gamma2m)) = -gamma2p*gammaSm + gamma2m


idealNumberOfPoints = 2000 ∷ Integer
idealEps = 0.5 ∷ Double
initialPt = DD 0 0
subdivisions = 5 ∷ Integer

filterAndCount ∷ (a→Bool) → [a] → (Integer, [a])
filterAndCount _ [] = (0,[])
filterAndCount p (x:xs) = if p x then (n+1,x:xs') else (n,xs')
    where (n,xs') = filterAndCount p xs

step ∷ (Double, Double, (Integer, [DD])) → (Double, Double, (Integer, [DD]))
step (eps, width, (count, pts)) = if count > idealNumberOfPoints then (eps/2,width,filterStep 2 eps pts) else (eps,width/fromInteger numberOfPts, (count*subdivisions^4, refinementStep numberOfPts width pts))

(endEps,endWidth, (endCount,refinedPts)) = head . dropWhile (\(eps,_,_) → eps > idealEps) . iterate step $ (10,1,(1,[initialPt]))

-- makeInterval start end num =


--------- S1P0 ratio = 2 points in the diagonal
-- lie on the circle (x-3)^2 + y^2 = 8
-- x = 2 sqrt 2 cos t +3
-- y = 2 sqrt 2 sin t
-- only need to check for t in pi/2 to 3pi/2
-- diagArc = map (\z → DD z z) $ [x t :+ y t | t <- ang ]
--     where
--      x t = 2 * sqrt 2 * cos t + 3
--      y t = 2 * sqrt 2 * sin t
--      angs = map




-- width = 1/fromInteger numberOfPts
--
-- interfill subs pt = filter (\z → normsq' z <= r2) $ [DD (x:+y) (z:+w) | x <- pts, y <- pts, z <- pts, w <- pts ]
--     where
--         r2 = 0.99 -- stay off the edge of the disc
--         pts = map (\k → (2*fromInteger k/fromInteger subs -1) *width) [0..subs]



mainRefine = do
    -- moduliPts <- readIn "S1P0levels/S1P0n2-" 200
    let start = 0
    let end = 50
    moduliPts <- readIns "integral_values15/values" start end

    let moduliPts' = map (filter refine') moduliPts
    writeOuts "S1P0levels15/S1P0n2-0.5-" start moduliPts'
    putStrLn "S1P0levels15/S1P0n2-0.5- written"

    -- writeOut "S1P0levels/S1P0n2-S0P1n2-" moduliPts'
    -- refinedPts <- readIns "S1P0levels/S1P0n2-" 650

    let refinedPts' = map (refine'' 4 0.5) moduliPts'
    writeOuts "S1P0levels15/S1P0n2-0.5-S0P1n2-0.5-" start refinedPts'
    putStrLn "S1P0levels15/S1P0n2-0.5-S0P1n2-0.5- written"

plotFiles ∷ Text → IO Bool
plotFiles path = do
    pts <- readIn path 1000 ∷ IO [(DD,Double,Double,Double)]
    let pts' = map (\(pt,_,_,_) → pt) pts
    mainGraph pts'


refine :: [(DD, Double, Double, Double)] → [(DD, Double, Double, Double)]
refine = filter refine'

-- S1P0 gamma_minus = 2
refine' :: (DD, Double, Double, Double) → Bool
refine' (pt, gammaSm, gamma2p, gamma2m) = 1.5 < gammaSm && gammaSm < 2.5

-- S0P1 gamma_minus = 2
refine'' :: Double → Double → [(DD, Double, Double, Double)] → [(DD, Double, Double, Double)]
refine'' val eps = filter (inRange (val-eps) (val+eps) . gammaPm)
    where
        gammaPm (pt, gammaSm, gamma2p, gamma2m) = -gamma2p*gammaSm + gamma2m












writeOuts :: Show a => Text → Integer → [[a]] → IO ()
writeOuts s n pts = do
    let fileNames = map (\n → FP.fromText $ s ++ show n ++ ".pts") [n..]
    let dir = directory . head $ fileNames
    makeFolderIfNeeded dir
    let files = zipWith writeFile fileNames (map show pts)
    sequence_ files

readIns :: Read a => Text → Integer → Integer → IO [[a]]
readIns s n m = do
    let fileNames = map (\n → FP.fromText $ s ++ show n ++ ".pts") [n..m]
    mapM extractFile fileNames

readInSome :: Read a => Text → Integer → IO [[a]]
readInSome s n = do
    let fileNames = map (\n → FP.fromText $ s ++ show n ++ ".pts") ([1..]::[Integer])
    let files = take n . drop1inBlah 2 $ fileNames
    mapM extractFile files

drop1inBlah n = drop1inBlah' n n
drop1inBlah' ∷ Integer → Integer → [a] → [a]
drop1inBlah' _ _ [] = []
drop1inBlah' n k (x:xs) = if k == 1 then x:drop1inBlah' n n xs else drop1inBlah' n (k-1) xs


















--- refine a data set, file by file
-- mainRefine = do
--     moduliPts <- readIns "integral_values/values" 3
--
--     let moduliPts' = map refine moduliPts
--
--     writeOut "S1P0levels/S1P0lvl" moduliPts'
--
--
--
--
--
-- refine :: [(DD, Double, Double, Double)] → [(DD, Double, Double)]
-- refine = map refine'
-- refine' :: (DD, Double, Double, Double) →
-- refine' (pt, thetaSm, theta2p, theta2m) = (pt, thetaSm)
--
-- readIns :: Read a => Text → Integer → IO [[a]]
-- readIns s n = do
--     let fileNames = map (\n → FP.fromText $ s ++ show n ++ ".pts") ([1..]::[Integer])
--     let files = take n fileNames
--     liftM sequence . map extractFile $ files









--
-- prepGridForGraph :: [(DD,Double, Double)] → [Graph3D Double Double Double]
-- prepGridForGraph = p3 . p2 . map p1
--
-- p3 :: [[[(DD,Int,Int)]]] → [Graph3D Double Double Double]
-- p3 = concatMap (map helper)
--     where
--         helper xs@((_,i1,i2):_) = Data3D (chooseColour i1 i2) stdOptions (map (projby.first) xs)
--         first (x,_,_) = x
--
-- p2 :: [(DD,Int,Int)] → [[[(DD,Int,Int)]]]
-- p2 = map (g3 . s3) . g2 . s2 . f
--     where
--         -- f = id
--         -- f = filter (\(_,i1,i2) → (0 <= i1 && i1 <= 0) && (-2 <= i2 && i2 <= -2))
--         f = filter (\(_,i1,i2) → let p = (i1,i2) in p == (0,-2) || p == (0,-1) || p == (0,-3))
--         s2 = sortBy (\(_,x,_) (_,y,_) → compare x y)
--         g2 = groupBy (\(_,x,_) (_,y,_) → x==y)
--         s3 = sortBy (\(_,_,x) (_,_,y) → compare x y)
--         g3 = groupBy (\(_,_,x) (_,_,y) → x==y)
--
--
-- p1 :: (DD,Double, Double) → (DD, Int, Int)
-- p1 (pt,d1, d2) = (pt, round d1, round d2)
--
-- chooseColour :: Int → Int → [Option]
-- chooseColour i1 i2 = [Color c, Style Dots]
--     where
--         e1 = even i1
--         e2 = even i2
--         c
--             | e1 && e2 = White
--             | e1 = Yellow
--             | e2 = Orange
--             | otherwise = Cyan
--         -- f (-1) = 128
--         -- f 0 = 196
--         -- f 1 = 256
--         -- f 2 = 128
--         -- f 3 = 256
--
-- mainColourfulGrid = do
--     pts <- readIn "moduli_data_grid_ints/moduliPtsRefined" 19
--     plot' [Interactive] X11 $
--         prepGridForGraph pts









-- SYMMETRIC CASE
-- mirrorPts = [DD a (-a) | a <- discPts]
-- heights :: [(Double, Double, Double)]
-- heights = map (\pt@(DD (x:+y) _) → (x,y,gamma_minus mt nt pt)) mirrorPts
-- heightsGraph :: IO Bool
-- heightsGraph = do
--     plot' [Interactive] X11 $
--         colourByZ heights
--         -- [ Data3D [Color White, Style Dots] stdOptions heights ]
--
--
-- colourByZ :: [(Double,Double,Double)] → [Graph3D Double Double Double]
-- colourByZ pts = map doit pts'
--     where
--         third (_,_,z) = z
--         s = sortBy (\pt1 pt2 → compare (third pt1) (third pt2))
--         g = groupBy (\pt1 pt2 → groupRange (third pt1) (third pt2))
--         pts' = g . s $ pts
--         small = third . head . head $ pts'
--         big = third . head . last $ pts'
--         -- f h = [Color (RGB 128 (round $ (h-small)/(big-small)*255) 128), Style Dots]
--         f h = [Color c, Style Dots]
--             where
--                 c
--                     | 1.5 < h = RGB 128 255 128
--                     | 0.5 < h && h <= 1.5 = RGB 128 196 128
--                     | -0.5 < h && h <= 0.5 = RGB 128 128 128
--                     | -1.5 < h && h <= -0.5 = RGB 128 64 128
--                     | h <= -1.5 = RGB 128 0 128
--         doit pts = Data3D (f (third . head $ pts)) stdOptions pts
--
-- ranges = [-1.5,-0.5,0.5,1.5]
-- groupRange = groupRange' ranges
-- groupRange' [] pt1 pt2 = True
-- groupRange' (l:ls) pt1 pt2
--     | pt1 <= l && pt2 <= l = True
--     | pt1 <= l || pt2 <= l = False
--     | otherwise = groupRange' ls pt1 pt2
--



-- find moduli points
-- this point for m=2,n=-1 has gamma- = 0.0007458963782396
-- startPt = DD sPtA sPtB
--
-- sPtA = (-0.6722063000184105) :+ (-0.4062275242242663)
-- sPtB = 0.6058897417550098 :+ (-0.3018682992022683)
--
-- -- how far to move from known good point to next
-- stepsize :: Double
-- stepsize = 0.05
-- -- how tolerant are we to accepting something as a soln
-- tol :: Double
-- tol = 0.3
--
--
-- getAdjPts (DD (x:+y) (z:+w)) = [(DD ((x+s):+y) (z:+w)),(DD (x:+(y+s)) (z:+w)),(DD (x:+y) ((z+s):+w)),(DD (x:+y) (z:+(w+s))),(DD ((x-s):+y) (z:+w)),(DD (x:+(y-s)) (z:+w)),(DD (x:+y) ((z-s):+w)),(DD (x:+y) (z:+(w-s)))]
--     where s = stepsize
--
-- -- an arbitrary ordering based on axis
-- compareDD :: DD → DD → Ordering
-- compareDD (DD (a0:+a1) (a2:+a3)) (DD (b0:+b1) (b2:+b3)) = (compare a0 b0) `pri` (compare a1 b1) `pri` (compare a2 b2) `pri` (compare a3 b3)
--
-- instance Ord DD where
--     compare = compareDD
--
-- -- priority, only use second argument in case of a tie
-- pri :: Ordering → Ordering → Ordering
-- pri EQ c2 = c2
-- pri c1 _ = c1
--
-- test pt = offDiag pt && inDisc pt && symcond' tol pt
--
--
-- testNext :: Set DD → [DD] → [DD]
-- testNext _ [] = []
-- testNext tested (x:goodPts) = x : testNext tested' (goodPts ++ newGoodPts)
--     where
--         newPts = filter (flip Set.notMember tested) . getAdjPts $ x
--         tested' =  insertList tested newPts
--         newGoodPts =  filter test newPts
--
-- insertList :: Ord a => Set a → [a] → Set a
-- insertList set [] = set
-- insertList set (x:xs) = insertList (Set.insert x set) xs
--
-- moduliPtsSearch = testNext (Set.singleton startPt) [startPt]
--
-- -- mainSearchGraph = mainReadAndGraph "moduli_data_02/moduliPtsSearch" 4
-- mainSearchGraph = do
--     moduliPts <- readIn "moduli_data_02/moduliPtsSearch" 4
--     let mirror (DD z w) = DD (-z) (-w)
--     let moduliPts' = map mirror moduliPts
--     mainGraphs projbx
--         [ boundaryPts
--         , symPts
--         , [startPt]
--         , moduliPts
--         , moduliPts'
--         ]
--
--
--
-- -- a little function to search out points solving the sym cond in a ball based near the given one
--
-- nearbyPts (DD a0 b0) eps = filter (symcond m n mt nt k kt eps) $ [ DD (a0 + x) (b0 + y) | x <- pts, y <- pts ]
--     where
--         pts = [fromPolar r t | r <- [0,0.01..0.1] , t <- [0,(2*pi/10)..(2*pi)]]









--- POINTS WHERE THE BRANCH CIRCLE PASSES THROUGH 1
center (ax:+ay) = 1 :+ (0.5*((ax-1)^2 + ay^2)/ay)
radius a = sqrt (normsq' (center a) - 1)
betas a = filter inDisc [ fromPolar r t | t <- [0,0.1 .. 6.2] ]
    where
        r = radius a
        inDisc = (< 0.99).normsq'

badBranchCircles = concat [map (DD a) (betas a) | a <- discPts 100]

mainBadBranchCircles = do
    moduliSpace <- readIn "moduli_data_02/moduliPtsSearch" 4
    mainGraphs projbx [map includeBoundary $ boundaryPts 50, map includeBoundary symPts, badBranchCircles, moduliSpace]












-- symPts = map (DD 1) s1pts ++ map (flip DD 1) s1pts ++ map (DD (-1)) s1pts ++ map (flip DD (-1)) s1pts







-- CUT POINTS






---- file utils

chunkify _ [] = []
chunkify n xs = y : chunkify n z
    where (y,z) = splitAt n xs

writeOut :: Show a => Text → [a] → IO ()
writeOut s pts= do
    let fileNames = map (\n → FP.fromText $ s ++ show n ++ ".pts") ([1..] :: [Integer])
    let dir = directory . head $ fileNames
    makeFolderIfNeeded dir
    let ptsChunks = chunkify 5000 pts
    let files = zipWith writeFile fileNames (map show ptsChunks)
    sequence_ files

extractFile :: Read a => FilePath → IO [a]
extractFile s = do
    bool <- F.isFile s
    let yup = do {
        text <- readFile s;
        return (read text)}
    if bool then yup else return []

readIn :: Read a => Text → Integer → IO [a]
readIn s n = do
    let fileNames = map (\n → FP.fromText $ s ++ show n ++ ".pts") ([1..]::[Integer])
    let files = take n fileNames
    liftM concat . mapM extractFile $ files

makeFolderIfNeeded :: FilePath → IO ()
makeFolderIfNeeded path = do
    bool <- F.isDirectory path
    if bool then return () else F.createTree path

--- graph that shit




mainReadAndGraph :: Text → Integer → IO Bool
mainReadAndGraph s n = do
    moduliPts <- readIn s n
    mainGraph moduliPts

mainGraph :: [DD] → IO Bool
mainGraph pts =  plot' [Interactive] X11
        [
        -- Data3D [Color White, Style Dots] [] (map projby pts)
        Data3D [Color Yellow, Style Dots] [] (map projbx pts)
        -- , Data3D [Color White, Style Dots] [] (map projby pts)
        -- , Data3D [Color Green, Style Points] [] (map projby [startPt])
        -- , Data3D [Color Blue, Style Dots] [] (map projbx boundaryPts)
        -- , Data3D [Color White, Style Dots] [] (map projbx symPts)
        -- , Data3D [Color White, Style Dots] [] (map projby pts)
        -- , Data3D [Color Green, Style Points] [] (map projay [startPt])
        -- , Data3D [Color Blue, Style Dots] [] (map projay boundaryPts)
        -- , Data3D [Color Green, Style Dots] [] (map projay conformalPts)
        -- , Data3D [Color White, Style Dots] [] (map projay singularPts)
        ]

colours = cycle [Blue, White, Yellow, Orange, LightGreen, LightBlue,LightRed]
mainGraphs :: (DD → (Double,Double,Double)) → [[DD]] → IO Bool
mainGraphs proj pts = do
    let zipper c ps = Data3D [Color c, Style Dots] stdOptions (map proj ps)
    plot' [Interactive] X11 $
        zipWith zipper colours pts
