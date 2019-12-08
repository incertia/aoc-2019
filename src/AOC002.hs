module AOC002
  ( solve002
  ) where

import Control.Lens (ix, (^?!))
import Control.Monad.State.Strict (execState)

import IntCode

solve002 :: String -> Bool -> String
solve002 i z = show $ let m = initialMachine [] $ toTape i in
  if z then let range = [0..99] in
    (\(a, b) -> 100 * a + b) . head . filter ((==19690720) . uncurry (doProblem m)) $ (,) <$> range <*> range
  else doProblem m 12 2

doProblem :: TapeMachine -> Int -> Int -> Int
doProblem m a b = execState (writeT 1 a >> writeT 2 b >> eval) m ^?! tape . ix 0
