module AOC004
  ( solve004
  ) where

import Data.List (group, sort)

solve004 :: String -> Bool -> String
solve004 i z = show $
  let (a, b) = split i
      x = read a :: Integer
      y = read b
      f = if z then (==2) else (>=2)
  in  fromIntegral . length . filter (any (f . length) . group) . filter (\x -> x == sort x) $ show <$> [x..y]

split :: String -> (String, String)
split ('-':xs) = ("", xs)
split (x:xs) = let (y, z) = split xs in (x:y, z)
