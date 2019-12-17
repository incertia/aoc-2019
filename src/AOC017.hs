{-# LANGUAGE LambdaCase #-}

module AOC017
  ( solve017
  ) where

import Control.Lens (view, (&), (<&>), (.~))
import Control.Monad (when)
import Control.Monad.State (execState)
import Data.Array (Array, listArray, bounds, (!))
import Data.Char (chr, ord)

import IntCode (initialMachine, toTape, tapeIn, tapeOut, eval, writeT)

intersections :: Array (Int, Int) Bool -> [(Int, Int)]
intersections a = [ (x, y) | x <- [minX + 1..maxX - 1], y <- [minY + 1..maxY - 1], f x y]
  where ((minX, minY), (maxX, maxY)) = bounds a
        f x y = a ! (x, y) && a ! (x - 1, y) && a ! (x + 1, y) && a ! (x, y - 1) && a ! (x, y + 1)

solve017 :: String -> Bool -> String
solve017 i b =
  if b then show $ r !! (length r - 1)
  else show . sum $ uncurry (*) <$> intersections a
  where r = view tapeOut . execState (when b (writeT 0 2) >> eval) $
              m & tapeIn .~ if b then ord <$> input else []
        m = initialMachine [] $ toTape i
        v = lines $ chr <$> r
        s = v <&> fmap (\case '#' -> True
                              '^' -> True
                              '>' -> True
                              'v' -> True
                              '<' -> True
                              _   -> False)
        w = head $ length <$> s
        h = length s - 1
        a = listArray ((0, 0), (h - 1, w - 1)) (concat s)
        routines = "A,B,A,C,A,B,C,B,C,B"
        aa = "R,8,L,10,L,12,R,4"
        bb = "R,8,L,12,R,4,R,4"
        cc = "R,8,L,10,R,8"
        input = unlines [routines, aa, bb, cc, "n"]
