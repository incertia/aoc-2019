{-# LANGUAGE TypeApplications #-}

module AOC009.Polysemy
  ( solve009poly
  ) where

import IntCodePolysemy (runMachineST, initialMachine, toTape)

solve009poly :: String -> Bool -> String
solve009poly i b = show $ r !! (length r - 1)
  where r = runMachineST m [if b then 2 else 1]
        m = initialMachine . toTape @Int $ i
