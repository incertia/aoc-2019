module AOC019
  ( solve019
  ) where

import Control.Lens (view, (&), (.~))
import Control.Monad.State (execState)

import IntCode (initialMachine, toTape, tapeIn, tapeOut, eval)

solve019 :: String -> Bool -> String
solve019 i b =
  if b then show $ 10000 * startx + goodh
  else show . length . filter (==1) $ r
  where r = concat $ runtapewithinput <$> inputs
        m = initialMachine [] $ toTape i
        inputs = fmap f $ (,) <$> [0..49] <*> [0..49]
        f (x, y) = [x, y]
        runtapewithinput x = view tapeOut . execState eval $ m & tapeIn .~ x
        good h = concatMap runtapewithinput [[x, h],
                                             [x + 99, h],
                                             [x, h + 99],
                                             [x + 99, h + 99]] == [1, 1, 1, 1]
          where base  = concatMap runtapewithinput $ (:[h]) <$> [0..]
                start = length (takeWhile (==0) base)
                l     = length (takeWhile (==1) $ dropWhile (==0) base)
                x     = start + l - 100
        ub = searchub 0
        searchub n = if good n then n else searchub (n + 100)
        bsearch lb ub | lb == ub  = lb
                      | good m    = bsearch lb m
                      | otherwise = bsearch (m + 1) ub
          where m = (lb + ub) `div` 2
        goodh = bsearch (ub - 100) ub
        startx = start + l - 100
          where base  = concatMap runtapewithinput $ (:[goodh]) <$> [0..]
                l     = length (takeWhile (==1) $ dropWhile (==0) base)
                start = length (takeWhile (==0) base)
