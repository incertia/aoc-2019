module AOC007
  ( solve007
  ) where

import Control.Lens (view, (&), (.~))
import Control.Monad.State (execState)
import Data.Foldable (foldl')
import Data.List (permutations, maximumBy)

import IntCode (initialMachine, toTape, tapeIn, tapeOut, eval)

solve007 :: String -> Bool -> String
solve007 i b = show . maximum $ chain <$> perms
  where chain p =
          -- thanks /u/goliatskipson for making me realize that this
          let r = foldl' (\rs p -> view tapeOut . execState eval $ m & tapeIn .~ p:rs) (0:r) p
          in  r !! (length r - 1)
          -- is equivalent to the following:
          -- let r1 = view tapeOut . execState eval $ m & tapeIn .~ p1:0:r5
          --     r2 = view tapeOut . execState eval $ m & tapeIn .~ p2:r1
          --     r3 = view tapeOut . execState eval $ m & tapeIn .~ p3:r2
          --     r4 = view tapeOut . execState eval $ m & tapeIn .~ p4:r3
          --     r5 = view tapeOut . execState eval $ m & tapeIn .~ p5:r4
          -- in  r5 !! (length r5 - 1)
          where m = initialMachine [] $ toTape i
        perms = permutations $ if b then [5..9] else [0..4]
