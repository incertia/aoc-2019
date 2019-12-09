module AOC009
  ( solve009
  ) where

import Control.Lens (view, (&), (.~))
import Control.Monad.State (execState)

import IntCode (initialMachine, toTape, tapeIn, tapeOut, eval)

solve009 :: String -> Bool -> String
solve009 i b = show $ r !! (length r - 1)
  where r = view tapeOut . execState eval $ m & tapeIn .~ [if b then 2 else 1]
        m = initialMachine [] $ toTape i
