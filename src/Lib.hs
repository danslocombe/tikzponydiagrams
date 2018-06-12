{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Text.LaTeX.Base.Syntax
import qualified Text.LaTeX.Packages.TikZ.Syntax as T
import Text.LaTeX.Base.Render as R
import Text.LaTeX.Packages.TikZ.Simple
import qualified Data.Text as TX
import Data.List
import Data.Maybe

writeWrite :: FilePath -> Cfg -> IO ()
writeWrite fn cfg = R.renderFile fn x
  where
    x :: LaTeX
    x = ((tikzpicture $ figuretikz $ conv cfg))

writeProject fn = writeWrite ("../report/tikz/" ++ fn ++ ".tex")

newtype Id = Id Int deriving (Show, Num, Eq, Ord)
type Rc = Int

data Object = Object 
  { getImm :: Bool 
  , getObjId :: Id 
  , getObjRow :: Int 
  , getObjCustomText :: String
  } deriving Show

getRawObjId o = let (Id id) = getObjId o in id

data Actor = Actor [Object] [(Id, Rc)] deriving Show
data RefType = Ref | CurveRef Double | NamedCurvedRef String | NamedRef String | DroppedRef | DroppedRefRev deriving Show

data Cfg = Cfg [Actor] [(Id, Id, RefType)] [(Id, Id, RefType)] deriving Show

objPointToFig :: (Object, Point) -> Figure
objPointToFig (o, p@(px, py)) = Figures 
  [ bg
  , text]
  where
    circleRad = 0.25
    squareSize = 0.5
    bg = if getImm o then Circle p circleRad
                else Rectangle (px - squareSize / 2, py + squareSize / 2) squareSize squareSize

    text = Text p $ if getObjCustomText o == "" then iotatext else customtext
    iotatext = TeXMath Dollar (TeXRaw $ TX.pack $  "\\iota_" ++ show (getRawObjId o))
    customtext = TeXMath Dollar (TeXRaw $ TX.pack $ getObjCustomText o)
    
organiseObjects :: [Object] -> Point ->  [(Object, Point)]
organiseObjects os (offsetx, offsety) = offset
  where
    grouped = groupBy (\o o' -> getObjRow o == getObjRow o') os
    sorted = sortBy (\xs ys -> let o = head xs in 
                              (let o' = head ys in
                     (compare (getObjRow o) (getObjRow o')))) grouped
    n = length sorted
    n' = fromIntegral n
    heighted = zipWith (\xs i -> (xs, fromIntegral i / (n' + 1))) sorted [1..]

    processRow :: ([Object], Double) -> [(Object, Point)]
    processRow (row, y) = let k = fromIntegral (length row) in 
      zipWith (\obj i -> (obj, ((fromIntegral i / (k + 1)), y))) row [1..]

    widthed = processRow <$> heighted

    all = concat widthed

    offset = (\(o, (x, y)) -> (o, (x * widthHeap + offsetx, -y * heightHeap + offsety))) <$> all

    

widthHeap = 3
heightHeap = 4
spacingHeap = 0.5

conv :: Cfg -> Figure
conv (Cfg hs fs afs) = Figures $ heaps ++ alphas ++ objs ++ fields ++ actorfields ++ rcs
  where
    n = length hs

    heaps = foldl (\ls i -> 
      (Rectangle ((widthHeap + spacingHeap) * fromIntegral i, 0) widthHeap heightHeap) : ls) [] [0..(n-1)]

    alphas = foldl (\ls i -> (Text (alphapos i) (alphatext i)) : ls) [] [0..(n-1)]

    widthdist = widthHeap + spacingHeap
    alphapos i = (widthHeap / 2 + (widthHeap + spacingHeap) * (fromIntegral i), 0.45)
    alphatext i = TeXMath Dollar (TeXRaw $ TX.pack $  "\\alpha_" ++ show i)

    offsets :: [Point]
    offsets = map (\i -> (fromIntegral i * (widthHeap + spacingHeap), fromIntegral 0)) [0..]

    f :: Actor -> Point -> [(Object, Point)]
    f (Actor os _) p = organiseObjects os p

    organised :: [(Object, Point)]
    organised = concatMap (uncurry f) (zip hs offsets)

    objs :: [Figure]
    objs = objPointToFig <$> organised

    fields = [genRef p1' p2' t
             | (o1, p1) <- organised,
               (o2, p2) <- organised,
               let mt = lookupRef fs (getObjId o1, getObjId o2),
               isJust mt,
               let (p1', p2') = modifyPs p1 p2
                   Just t = mt]

    actorfields = [ genRef ap' op' t
                  | (o, op) <- organised,
                    (ap, aid) <- map (\i -> (alphapos i, Id i)) [0..(n-1)],
                    let mt = lookupRef afs (aid, getObjId o),
                    isJust mt,
                    let (ap', op') = modifyPs ap op
                        Just t = mt
                  ]
                    
    rcs = zipWith3 makeRcs hs [0..] (map rcpos [0..])
    rcpos i = (widthHeap / 2 + (widthHeap + spacingHeap) * (fromIntegral i), -heightHeap) 

lookupRef :: [(Id, Id, RefType)] -> (Id, Id) -> Maybe RefType
lookupRef xs (id, id') = listToMaybe [ t | (x, x', t) <- xs, x == id, x' == id']


midpoint :: Point -> Point -> Point
midpoint (x, y) (x', y') = ((x + x') / 2, (y + y') / 2)

midpointWeight :: Double -> Double -> Point -> Point -> Point
midpointWeight kx ky (x, y) (x', y') = ((x * kx + x' * kx'), (y * ky + y' * ky'))
  where
    kx' = (1-kx)
    ky' = (1-ky)

makeCurveRefMid :: Double -> Point -> Point -> [Point]
makeCurveRefMid c p0@(x0, y0) p1@(x1, y1) = [pp0, pp1]-- [pp0, (x, y), pp1]
  where
    m@(mx, my) = midpoint p0 p1
    dir = atan2 (y1 - y0) (x1 - x0)
    dirmod = dir + c
    dist = 0.35
    x = mx + dist * cos(dirmod)
    y = my + dist * sin(dirmod)

    pp0 = midpointWeight (sqrt 2 / 2) 0.5 p0 (x, y)
    pp1 = midpointWeight (1 - sqrt 2 / 2) 0.5 (x, y) p1

makeCurveRefFinal :: Double -> Point -> Point -> Point
makeCurveRefFinal c (x0, y0) (x1, y1) = (x, y)
  where
    (mx, my) = ((x0 + x1) / 2, (y0 + y1) / 2)
    dir = atan2 (y1 - y0) (x1 - x0)
    dirmod = dir + c
    dist = 0.09
    x = x1 + dist * cos(dirmod)
    y = y1 + dist * sin(dirmod)

genRef :: Point -> Point -> RefType -> Figure
genRef p0 p1 Ref = Arrow [p0, p1]
genRef p0 p1 (CurveRef c) = Curvy $ [p0] ++ makeCurveRefMid c p0 p1 ++ [makeCurveRefFinal c p0 p1]
genRef p0@(x0, y0) p1@(x1, y1) DroppedRef
  = Figures [ Arrow [p0, p1]
            , Line [strikeStart, strikeEnd]
            , Line [strikeStart', strikeEnd' ]
            ]
  where
    (mx, my) = ((x0 + x1) / 2, (y0 + y1) / 2)
    strikeLen = 0.126
    doubledist = -0.08
    strikeStart = (mx + strikeLen, my - strikeLen)
    strikeEnd = (mx - strikeLen, my + strikeLen)
    strikeStart' = (mx + strikeLen, my - strikeLen + doubledist)
    strikeEnd' = (mx - strikeLen, my + strikeLen + doubledist)

genRef p0@(x0, y0) p1@(x1, y1) (NamedRef name)
  = Figures [ Arrow [p0, p1]
            , Text textPos text
            ]
  where
    text = TeXRaw $ TX.pack name
    (mx, my) = midpoint p0 p1
    textOffset = 0.25
    dir = atan2 (y1 - y0) (x1 - x0)
    dir2 = dir + pi / 2
    textPos = (mx + cos(dir2) *textOffset, my + sin(dir2) * textOffset)

genRef p0@(x0, y0) p1@(x1, y1) (NamedCurvedRef name)
  = Figures [ Text textPos text
            , Curvy $ [p0] ++ makeCurveRefMid 1 p0 p1 ++ [makeCurveRefFinal 1 p0 p1]
            ]
  where
    text = TeXRaw $ TX.pack name
    (mx, my) = midpoint p0 p1
    textOffset = 0.35
    dir = atan2 (y1 - y0) (x1 - x0)
    dir2 = dir + pi / 2
    textPos = (mx + cos(dir2) *textOffset, my + sin(dir2) * textOffset)

-- genRef p0@(x0, y0) p1@(x1, y1) DroppedRefRev
--   = Figures [ Arrow [p0, p1]
--             , Line [strikeStart, strikeEnd]
--             , Line [strikeStart', strikeEnd' ]
--             ]
--   where
--     (mx, my) = ((x0 + x1) / 2, (y0 + y1) / 2)
--     strikeLen = 0.126
--     doubledist = -0.08
--     strikeStart = (mx + strikeLen, my + strikeLen)
--     strikeEnd = (mx - strikeLen, my - strikeLen)
--     strikeStart' = (mx + strikeLen, my + strikeLen + doubledist)
--     strikeEnd' = (mx - strikeLen, my - strikeLen + doubledist)

makeRcs :: Actor -> Int -> Point -> Figure
makeRcs (Actor _ rs) aid (x,y) = Figures ret
  where
    spacing = 0.5
    spacingInit = 0.5
    poses = map (\i -> (x, -i*spacing + y)) [1..]

    ret = zipWith (\(oid, rc) pos -> Text pos (text oid rc)) rs poses

    text (Id oid) rc = TeXMath Dollar $ TeXRaw $ TX.pack $ 
      "\\alpha_" ++ show aid ++
      ".rc(\\iota_" ++ show oid ++
      ") = " ++ show rc




sqr x = x * x

sign x = case () of
  () | x > 0 -> 1
  () | x < 0 -> -1
  () -> 0

modifyPs (x1, y1) (x2, y2) = (p1', p2')
  where
    length = sqrt $ sqr (x2 - x1) + sqr (y2 - y1)
    -- angle = sign(x2 - x1) * atan2 (y2 - y1) (x2 - x1)
    angle = atan2 (y2 - y1) (x2 - x1)
    dist = 0.3
    p1' = (x1 + dist * cos angle, y1 + dist * sin angle)
    p2' = (x2 - dist * cos angle, y2 - dist * sin angle)

mkMut id row = Object False id row ""
mkImm id row = Object True id row ""
mkImmCustom id row cust = Object True id row cust

-- cfg0 = Cfg [Actor [mkImm 0 0] [(0,0)], Actor [mkMut 2 0, mkImm 1 1] []] [(0, 1)] []
-- 
-- cfg1 = Cfg [Actor [mkImm 0 0] [(0,0), (1, 1), (2, 0)],
--             Actor [mkImm 1 0, mkImm 2 1] [(0,0), (1, 1), (2, 0)]
--             ] [(0, 1), (1, 2)] [(0, 0, Ref)]
-- 
-- cfgCollectZeroRC0 = 
--   Cfg [ Actor [mkImmCustom 0 0 "U"] [(0,0), (1, 1), (2, 0)]
--       , Actor [mkImmCustom 1 0 "U", mkImmCustom 2 1 "R"] [(0,0), (1, 1), (2, 0)]
--       ] [(0, 1), (1, 2)] [(0, 0, DroppedRef), (0, 2, Ref)]
-- 
-- cfgCollectZeroRC1 = 
--   Cfg [ Actor [] [(1, 0), (2, 1)]
--       , Actor [mkImm 1 0 , mkImm 2 1 ] [(1, 0), (2, 1)]
--       ] [(1, 2)] [(0, 2, Ref)]
-- 
-- cfgprotect = Cfg [ Actor [mkImm 0 0, mkImm 4 1, mkImm 3 1] []
--                  , Actor [mkImm 1 0, mkImm 2 1] [] 
--                  ]
--                  [(0, 1), (1, 2), (2, 3), (3, 4)] [(0, 0, Ref)]
-- 
-- cfglb = Cfg [ Actor [mkImm 0 0, mkImm 1 1] [(2, 1)]
--             , Actor [mkImm 2 0, mkImm 3 1] [(2, 1), (4, 1)]
--             , Actor [mkImm 4 0, mkImm 5 1] [(4, 1)]] 
--                  [(0, 1), (1, 2), (2, 3), (3, 4), (4, 5)] [(0, 0, Ref)]
-- 
-- cfgSend0 = Cfg [ Actor [mkMut 0 0, mkImm 1 1, mkImm 3 2] [(2, 2)]
--               , Actor [mkImm 4 1, mkImm 2 0] [(2, 2)] 
--               , Actor [] []
--               ]
--               [(0, 1), (0, 2), (1, 3), (2, 4)] [(0, 0, Ref)]
-- 
-- cfgSend1 = Cfg [ Actor [mkMut 0 0, mkImm 1 1, mkImm 3 2] [(0, 1), (1, 1), (2, 1)]
--               , Actor [mkImm 2 0, mkImm 4 1] [(2, 2)] 
--               , Actor [] [(0,1), (1, 1), (2, 1)]
--               ]
--               [(0, 1), (0, 2), (1, 3), (2, 4)] [(0, 0, DroppedRef), (2, 0, Ref)]
-- 
-- 
-- cfgSendZeroRC0 = 
--   Cfg [ Actor [mkImm 0 0] [(0,0), (1, 1), (2, 0)]
--       , Actor [mkImm 1 0, mkImm 2 1] [(0,0), (1, 1), (2, 0)]
--       , Actor [] []
--       ] [(0, 1), (1, 2)] [(0, 0, Ref), (0, 2, Ref)]
-- 
-- cfgSendZeroRC1 = 
--   Cfg [ Actor [mkImm 0 0] [(0,0), (1, 1), (2, 255)]
--       , Actor [mkImm 1 0, mkImm 2 1] [(0,0), (1, 1), (2, 256)]
--       , Actor [] [(2, 1)]
--       ] [(0, 1), (1, 2)] [(0, 0, Ref), (2, 2, Ref)]
-- 
-- cfgsubref0 =
--   Cfg [ Actor [mkImm 0 0] [(1, 1)]
--       , Actor [mkImm 1 0, mkImm 2 0] [(1,1), (3, 1)]
--       , Actor [mkImm 3 0] [(3, 1)]
--       ] [(0, 1), (1, 2), (2, 3)] [(0, 0, Ref)]
-- 
-- cfgsubref1 =
--   Cfg [ Actor [mkImm 0 0] [(1, 1)]
--       , Actor [mkImm 1 0, mkImm 2 0] [(1,1), (3, 1)]
--       , Actor [mkImm 3 0] [(3, 1)]
--       ] [(0, 1), (1, 2), (2, 3)] [(0, 0, DroppedRef), (0, 2, Ref)]
-- 
-- cfgsubref2 =
--   Cfg [ Actor [] [(2, 1)]
--       , Actor [mkImm 1 0, mkImm 2 0] [(1,0), (2, 1), (3, 1)]
--       , Actor [mkImm 3 0] [(3, 1)]
--       ] [(0, 1), (1, 2), (2, 3)] [(0, 2, Ref)]
-- 
-- 
-- cfgsubrefbad =
--   Cfg [ Actor [] [(0, 1)]
--       , Actor [mkImm 0 0, mkImm 1 0, mkImm 2 1, mkImm 3 1] [(0,1), (4, 1)]
--       , Actor [mkImm 4 0] [(4, 1)]
--       ] [(0, 1), (1, 4), (2, 3), (3, 4)] [(0, 0, DroppedRefRev), (0, 2, Ref)]
--

refTag = NamedRef "\\tiny{Tag}"
refRef = NamedRef "\\tiny{Ref}"
refBox = NamedRef "\\tiny{Box}"
refVal = NamedRef "\\tiny{Val}"
refIso = NamedRef "\\tiny{Iso}"
curveRefTag = NamedCurvedRef "\\tiny{Tag}"
curveRefRef = NamedCurvedRef "\\tiny{Ref}"
curveRefBox = NamedCurvedRef "\\tiny{Box}"
curveRefVal = NamedCurvedRef "\\tiny{Val}"
curveRefIso = NamedCurvedRef "\\tiny{Iso}"

cfgex = 
  Cfg [ Actor [mkMut 0 1, mkMut 1 1, mkMut 2 0, mkImm 3 2] []
      ] [(0, 2, NamedRef "\\tiny{Box}"), (0, 3, NamedRef "\\tiny{Box}")
        ,(1, 2, NamedRef "\\tiny{Ref}"), (1, 3, NamedRef "\\tiny{Box}")] [(0, 0, Ref), (0, 1, Ref)]

cfgleak0 = 
  Cfg [ Actor [mkImm 0 1] [], Actor [mkImm 1 1] [] ]
      []
      [(0,0,refIso), (1,1,refIso)]
cfgleak1 = 
  Cfg [ Actor [mkImm 0 1] [], Actor [mkImm 1 1] [] ]
      []
      [(0,0,refIso), (0,1,refIso)]
cfgleak2 = 
  Cfg [ Actor [mkImm 0 1] [], Actor [mkImm 1 1] [] ]
      [(0,1,curveRefIso), (1, 0, curveRefTag)]
      [(0,0,refIso)]
cfgleak3 = 
  Cfg [ Actor [mkImm 0 1] [], Actor [mkImm 1 1] [] ]
      [(0,1,curveRefIso), (1, 0, curveRefTag)]
      [(1,0,refVal)]
cfgleak4 = 
  Cfg [ Actor [mkImm 0 1] [], Actor [mkImm 1 1] [] ]
      [(0,1,curveRefIso), (1, 0, curveRefTag)]
      []


cfgBasic  = Cfg [ Actor [mkImm 0 1] []] [] [(0,0,Ref)]

cfgbb0 = 
  Cfg [ Actor [mkMut 0 1] [(0, 0)], Actor [] [(0, 0)] ]
      []
      [(0,0,Ref)]

cfgbb1 = 
  Cfg [ Actor [mkMut 0 1] [(0, 1)], Actor [] [(0, 1)] ]
      []
      [(1,0,Ref)]

cfgbb2 =
  Cfg [ Actor [mkMut 0 1] [(0, 1), (1, 0)], Actor [mkMut 1 1] [(0, 1), (1, 0)] ]
      []
      [(1,1,Ref)]

cfgbb3 =
  Cfg [ Actor [mkMut 0 1] [(0, 0), (1, 0)], Actor [mkMut 1 1] [(0, 0), (1, 0)] ]
      []
      [(1,1,Ref)]

cfgbb4 =
  Cfg [ Actor [] [(1, 0)], Actor [mkMut 1 1] [(1, 0)] ]
      []
      [(1,1,Ref)]
      
