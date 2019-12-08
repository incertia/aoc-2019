{-# LANGUAGE LambdaCase #-}

module AOC008
  ( solve008
  ) where

import Data.Char (isSpace)
import Data.Foldable (foldl')
import Data.List (minimumBy, intercalate)
import Data.Ord (comparing)

solve008 :: String -> Bool -> String
solve008 i z =
  if z then
     intercalate "\n" $ concatMap (\case {1 -> "*"; _ -> " "}) <$>
       chunks w (foldl' (zipWith $ \a b -> if a == 2 then b else a)
                        (repeat 2)
                        layers)
  else show . (\x -> length (filter (==1) x) * length (filter (==2) x))
            . minimumBy (comparing (length . filter (==0))) $ layers
  where layers = chunks (w * h) (read . pure <$> filter (not . isSpace) i)
        (w, h) = (25, 6)
        chunks _ [] = []
        chunks n xs = take n xs : chunks n (drop n xs)
