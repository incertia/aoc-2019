{-# LANGUAGE TypeApplications #-}

module AOC009
  ( solve009
  ) where

import IntCodePolysemy (runMachine, initialMachine, toTape)

solve009 :: String -> Bool -> String
solve009 i b = show $ r !! (length r - 1)
  where r = runMachine m [if b then 2 else 1]
        m = initialMachine . toTape @Int $ i
