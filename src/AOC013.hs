module AOC013
  ( solve013
  ) where

import Control.Lens (view, _3, (&), (.~))
import Control.Monad.State (execState)
import Data.List.Split (chunksOf)

import IntCode (initialMachine, toTape, tapeIn, tapeOut, eval)

solve013 :: String -> Bool -> String
solve013 i b =
  if b then undefined
  else show $ length (filter ((==2) . view _3) tiles)
  where r = view tapeOut . execState eval $ m & tapeIn .~ []
        m = initialMachine [] $ toTape i
        x = chunksOf 3 r
        f [a, b, c] = (a, b, c)
        tiles = f <$> x
