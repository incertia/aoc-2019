{-# LANGUAGE LambdaCase #-}

module AOC003
  ( solve003
  ) where

import Data.Maybe (catMaybes)
import Text.Read (Read(..), get)

data Vec = U Integer
         | R Integer
data Seg = H (Integer, Integer) Integer
         | V (Integer, Integer) Integer

intersect :: Seg -> Seg -> Maybe (Integer, Integer)
intersect (H (x1, y1) l1) (V (x2, y2) l2) =
  if inrange x2 x1 (x1 + l1) && inrange y1 y2 (y2 + l2) then Just (x2, y1) else Nothing
intersect v@V{} h@H{} = h `intersect` v
intersect _ _ = Nothing

instance Read Vec where
  readPrec = get >>= flip fmap readPrec . (\case {'U' -> U; 'D' -> U . negate; 'L' -> R . negate; 'R' -> R})

mkSegs :: (Integer, Integer) -> [Vec] -> [Seg]
mkSegs _ [] = []
mkSegs p@(x, y) (U z : vs) = V p z : mkSegs (x, y + z) vs
mkSegs p@(x, y) (R z : vs) = H p z : mkSegs (x + z, y) vs

dist :: [Seg] -> (Integer, Integer) -> Integer
dist (H (x, y) l : s) p@(x', y') = if y == y' && inrange x' x (x + l) then abs (x - x') else abs l + dist s p
dist (V (x, y) l : s) p@(x', y') = if x == x' && inrange y' y (y + l) then abs (y - y') else abs l + dist s p

inrange :: Integer -> Integer -> Integer -> Bool
inrange x a b = x >= min a b && x <= max a b

solve003 :: String -> Bool -> Integer
solve003 i z =
  let [l1, l2] = lines i
      a = mkSegs (0, 0) $ read $ "[" ++ l1 ++ "]"
      b = mkSegs (0, 0) $ read $ "[" ++ l2 ++ "]"
      f = if z then \p -> dist a p + dist b p
          else \(a, b) -> abs a + abs b
  in  minimum . fmap f . catMaybes $ intersect <$> a <*> b
