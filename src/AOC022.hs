module AOC022
  ( solve022
  ) where

import Data.Foldable (foldl')
import GHC.Natural (Natural)
import Math.NumberTheory.Moduli (SomeMod(..), modulo, getVal)

data Shuffle = Cut  SomeMod
             | DWI  SomeMod
             | DINS
  deriving (Show, Eq)

solve022 :: String -> Bool -> String
solve022 i b = show $
  if b then ansB
  else ansA
  where f ["cut", x] = Cut (read x `modulo` n)
        f ["deal", "with", "increment", x] = DWI (read x `modulo` n)
        f ["deal", "into", "new", "stack"] = DINS
        shuffle = f . words <$> lines i
        n = if not b then 10007 else 119315717514047
        times = if not b then 1 else 101741582076661
        baseInit = 0 `modulo` n
        incInit = 1 `modulo` n
        (base', inc') = foldl' (apply n) (baseInit, incInit) shuffle
        base = base' * (1 - inc'^times) / (1 - inc')
        inc = inc' ^ times
        apply n (b, i) (Cut c) = (b + c * i, i)
        apply n (b, i) (DWI c) = (b, i / c)
        apply n (b, i) DINS    = (b - i, -i)
        ansA = case (2019 - base) / inc of
                    SomeMod k -> getVal k
                    _         -> error "impossible"
        ansB = case base + 2020 * inc of
                    SomeMod k -> getVal k
                    _         -> error "impossible"
