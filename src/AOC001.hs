module AOC001
  ( solve001
  )
  where

solve001 :: String -> Bool -> String
solve001 i b = show . sum . fmap (g . read) . lines $ i
  where f = (+(-2)) . (`div` 3)
        g = if b then sum . takeWhile (>0) . tail . iterate f else f
