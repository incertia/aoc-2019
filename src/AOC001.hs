module AOC001
  ( solve001
  )
  where

solve001 :: String -> Bool -> Integer
solve001 i b = sum . fmap (g . read) . lines $ i
  where f = (+(-2)) . (`div` 3)
        g = if b then sum . takeWhile (>0) . tail . iterate f else f
