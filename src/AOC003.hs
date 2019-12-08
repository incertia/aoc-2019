{-# LANGUAGE LambdaCase #-}

module AOC003
  ( solve003
  ) where

import Data.Maybe (catMaybes)
import Text.Read (Read(..), get)

data Vec = U Integer
         | R Integer
data Seg = H (Integer, Integer) Integer Integer
         | V (Integer, Integer) Integer Integer

intersect :: Bool -> Seg -> Seg -> Maybe Integer
intersect z (H (x1, y1) l1 d1) (V (x2, y2) l2 d2) =
  if inrange x2 x1 (x1 + l1) && inrange y1 y2 (y2 + l2) then Just $
     if z then d1 + d2 + abs (x1 - x2) + abs (y1 - y2)
     else abs x2 + abs y1
  else Nothing
intersect z v@V{} h@H{} = intersect z h v
intersect _ _ _ = Nothing

instance Read Vec where
  readPrec = get >>= flip fmap readPrec . (\case {'U' -> U; 'D' -> U . negate; 'L' -> R . negate; 'R' -> R})

mkSegs :: (Integer, Integer) -> Integer -> [Vec] -> [Seg]
mkSegs _ _ [] = []
mkSegs p@(x, y) d (U z : vs) = V p z d : mkSegs (x, y + z) (d + abs z) vs
mkSegs p@(x, y) d (R z : vs) = H p z d : mkSegs (x + z, y) (d + abs z) vs

inrange :: Integer -> Integer -> Integer -> Bool
inrange x a b = x >= min a b && x <= max a b

solve003 :: String -> Bool -> String
solve003 i z = show $
  let [l1, l2] = lines i
      a = mkSegs (0, 0) 0 $ read $ "[" ++ l1 ++ "]"
      b = mkSegs (0, 0) 0 $ read $ "[" ++ l2 ++ "]"
  in  minimum . catMaybes $ intersect z <$> a <*> b
