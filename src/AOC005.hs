module AOC005
  ( solve005
  ) where

import Control.Lens (view)
import Control.Monad.State.Strict (execState)

import IntCode

solve005 :: String -> Bool -> String
solve005 i b = show $
  let r = view tapeOut . execState eval . initialMachine [z] $ toTape i
      z = if b then 5 else 1
  in  r !! (length r - 1)
