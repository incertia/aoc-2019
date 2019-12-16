module AOC016
  ( solve016
  ) where

import Data.Array
import Data.List (scanl')
import Data.Foldable (foldl')

pat :: [Int]
pat = cycle [0, 1, 0, -1]

-- this will OOM on part B
-- the alternative is to notice that the offset is very high so the FFT
-- multiplication is just a bunch of 1s... but that's not the "idiomatic" FFT so
-- we don't actually code it
fft :: [Int] -> [Int]
fft v = [abs (sum [getsum a b * c | (a, b, c) <- takeWhile inbounds (asdf i)]) `mod` 10 | i <- [1..l]]
  where ps = psums v
        l = length v
        stuff = takeWhile inbounds . asdf <$> [1..l]
        inbounds (r, _, _) = r <= l
        getsum a b = ps ! min l b - ps ! a

psums :: [Int] -> Array Int Int
psums v = listArray (0, length v) $ scanl' (+) 0 v

switches :: Int -> [Int]
switches i = [i - 1, i + i - 1..]

asdf :: Int -> [(Int, Int, Int)]
asdf n = zip3 (0 : switches n) (switches n) pat

solve016 :: String -> Bool -> String
solve016 i b = show output
  where nums = read . pure <$> show (read i :: Integer)
        out = foldl' (const . fft) input stuff
        offset = if b then read . concatMap show $ take 7 nums else 0
        realinput = concat $ replicate 10000 nums
        input = if b then realinput else nums
        output = take 8 . drop offset $ out
        stuff = replicate 100 ()
