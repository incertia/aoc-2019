module AOC002
  ( solve002
  ) where

import Control.Lens (ix, view, (^?!))
import Control.Monad.State.Strict (execState)

import IntCode

solve002 :: String -> Bool -> Integer
solve002 i b = let m = initialMachine [] $ toTape i in
  if b then let range = [0..99] in
    (\(a, b) -> 100 * a + b) . head . filter ((==19690720) . uncurry (doProblem m)) $ (,) <$> range <*> range
  else doProblem m 12 2

doProblem :: TapeMachine -> Integer -> Integer -> Integer
doProblem m a b = execState (writeT 1 a >> writeT 2 b >> eval) m ^?! tape . ix 0
