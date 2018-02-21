{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Text.LaTeX.Base.Syntax
import qualified Text.LaTeX.Packages.TikZ.Syntax as T
import Text.LaTeX.Base.Render as R
import Text.LaTeX.Packages.TikZ.Simple
import qualified Data.Text as TX
import Data.List

writeWrite :: FilePath -> Cfg -> IO ()
writeWrite fn cfg = R.renderFile fn x
  where
    x :: LaTeX
    x = ((tikzpicture $ figuretikz $ conv cfg))

newtype Id = Id Int deriving (Show, Num, Eq, Ord)

data Object = Object Bool Id Int deriving Show
newtype Heap = Heap [Object] deriving Show

objPointToFig :: (Object, Point) -> Figure
objPointToFig (Object imm (Id id) _, p@(px, py)) = Figures 
  [ bg
  , text]
  where
    circleRad = 0.25
    squareSize = 0.5
    bg = if imm then Circle p circleRad
                else Rectangle (px - squareSize / 2, py + squareSize / 2) squareSize squareSize
    iotatext = TeXMath Dollar (TeXRaw $ TX.pack $  "\\iota_" ++ show id)
    text = Text p iotatext
    
organiseObjects :: [Object] -> Point ->  [(Object, Point)]
organiseObjects os (offsetx, offsety) = offset
  where
    grouped = groupBy (\(Object _ _ r) (Object _ _ r') -> r == r') os
    sorted = sortBy (\xs ys -> let (Object _ _ r) = head xs in 
                              (let (Object _ _ r') = head ys in
                     (compare r r'))) grouped
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
conv (Cfg hs fs) = Figures $ heaps ++ alphas ++ objs ++ fields
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

    f :: Heap -> Point -> [(Object, Point)]
    f (Heap os) p = organiseObjects os p

    organised :: [(Object, Point)]
    organised = concatMap (uncurry f) (zip hs offsets)

    objs :: [Figure]
    objs = objPointToFig <$> organised

    fields = [let (p1',p2') = modifyPs p1 p2 in Arrow [p1', p2']
             | ((Object _ id1 _), p1) <- organised,
               ((Object _ id2 _), p2) <- organised,
               (id1, id2) `elem` fs]

sqr x = x * x

modifyPs (x1, y1) (x2, y2) = (p1', p2')
  where
    length = sqrt $ sqr (x2 - x1) + sqr (y2 - y1)
    angle = atan2 (y2 - y1) (x2 - x1)
    dist = 0.6
    p1' = (x1 + dist * cos angle, y1 + dist * sin angle)
    p2' = (x2 - dist * cos angle, y2 - dist * sin angle)

data Cfg = Cfg [Heap] [(Id, Id)] deriving Show

cfg0 = Cfg [Heap [Object True 0 0], Heap [Object False 2 0, Object True 1 1]] [(0, 1)]
