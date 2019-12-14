module AOC011
  ( solve011
  ) where

import Control.Lens
import Control.Monad.State
import Data.Foldable (foldl')
import IntCode

solve011 :: String -> Bool -> String
solve011 i b =
  if b then undefined
  else undefined
  --where odds [x] = [x]
  --      odds (x:_:xs) = x : odds xs
  --      dirs = odds . tail
  --      colors = odds output
  --      dl = dirs output
  --      output = view tapeOut . execState eval $ m & tapeIn .~ 0:inputs
  --      inputs = undefined
  --      positions = foldl' travel ((0, 0), (0, 1)) dirs
  --      travel ((x, y), (dx, dy)) d =
  --        let (ndx, ndy) = case d of
  --                              0 -> (-y, x)
  --                              1 -> (y, -x)
  --                              _ -> undefined
  --        in ((x + ndx, y + ndy), (ndx, ndy))
  --      m = undefined
