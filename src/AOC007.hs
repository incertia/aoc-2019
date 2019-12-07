module AOC007
  ( solve007
  ) where

import Control.Lens (view, (&), (.~))
import Data.List (permutations)
import Control.Monad.State (execState)

import IntCode (initialMachine, toTape, tapeIn, tapeOut, eval)

solve007 :: String -> Bool -> Integer
solve007 i b =
  if b then maximum $ chain <$> perms2
  else maximum $ chain <$> perms1
  where chain :: [Integer] -> Integer
        chain [p1,p2,p3,p4,p5] =
          let r1 = view tapeOut . execState eval $ m & tapeIn .~ p1:0:r5
              r2 = view tapeOut . execState eval $ m & tapeIn .~ p2:r1
              r3 = view tapeOut . execState eval $ m & tapeIn .~ p3:r2
              r4 = view tapeOut . execState eval $ m & tapeIn .~ p4:r3
              r5 = view tapeOut . execState eval $ m & tapeIn .~ p5:r4
          in  r5 !! (length r5 - 1)
          where m = initialMachine [] $ toTape i
        perms1 = permutations [0..4]
        perms2 = fmap (5+) <$> perms1
