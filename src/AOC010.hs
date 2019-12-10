{-# LANGUAGE TupleSections #-}

module AOC010
  ( solve010
  ) where

import Control.Lens (_1, (%~))
import Data.List (maximumBy, sortBy, groupBy, nub)
import Data.Ord (comparing)

type Pt = (Int, Int)

solve010 :: String -> Bool -> String
solve010 i b =
  if b then show . (\(a, b) -> 100 * a + b) $ step targets !! 199
  else show . visible $ c
  where rows = fmap (zip [0..]) . lines $ i
        cols = concat $ zipWith (\y -> fmap (_1 %~ (,y))) [0..] rows
        roids = fst <$> filter ((=='#') . snd) cols
        visible p = length $ vecs p
        vecs p = nub $ vec p <$> filter (/=p) roids
        targets = sortBy (taxicab c) <$> groupBy samedir sorted
        sorted = sortBy (winding c) $ filter (/=c) roids
        samedir a b = vec c a == vec c b
        c = maximumBy (comparing visible) roids

step :: [[a]] -> [a]
step [] = []
step xs = (head <$> xs) ++ step rest
  where rest = filter (not . null) $ tail <$> xs

taxicab :: Pt -> Pt -> Pt -> Ordering
taxicab (x, y) (a, b) (c, d) = (abs (x - a) + abs (y - b)) `compare` (abs (x - c) + abs (y - d))

winding :: Pt -> Pt -> Pt -> Ordering
winding (cx, cy) (ax, ay) (bx, by) =
  let (adx, ady) = (ax - cx, ay - cy)
      (bdx, bdy) = (bx - cx, by - cy)
      a = adx < 0
      b = bdx < 0
  in if a /= b then bdx `compare` adx
     else if adx == 0 && bdx == 0 then
       if signum ady /= signum bdy then ady `compare` bdy
       else abs ady `compare` abs bdy
     else if adx == 0 && ady == -1 then LT
     else (adx * (-bdy)) `compare` (-ady * bdx)

vec :: Pt -> Pt -> Pt
vec (x, y) (u, v) =
  let dx = u - x
      dy = v - y
      d = gcd dx dy
  in  (dx `div` d, dy `div` d)
