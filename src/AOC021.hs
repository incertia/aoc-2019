module AOC021
  ( solve021
  ) where

import Control.Lens (view, (&), (.~))
import Control.Monad.State (execState)
import Data.Char (chr, ord)

import IntCode (initialMachine, toTape, tapeIn, tapeOut, eval)

solve021 :: String -> Bool -> String
solve021 i b = show $ r !! (length r - 1)
  where r = view tapeOut . execState eval $ m & tapeIn .~ if b then inputB else inputA
        m = initialMachine [] $ toTape i
        -- if there is a hole in A/B/C but D is solid, we jump
        inputA = ord <$> unlines [ "NOT A T"
                                 , "OR  T J"
                                 , "NOT B T"
                                 , "OR  T J"
                                 , "NOT C T"
                                 , "OR  T J"
                                 , "AND D J"
                                 , "WALK"
                                 ]
        -- if there is a hole in A/B/C but D is solid, we have to be able to
        -- walk again (E) or jump again (H)
        inputB = ord <$> unlines [ "NOT A T"
                                 , "OR  T J"
                                 , "NOT B T"
                                 , "OR  T J"
                                 , "NOT C T"
                                 , "OR  T J"
                                 , "AND D J"
                                 , "NOT E T"
                                 , "NOT T T"
                                 , "OR  H T"
                                 , "AND T J"
                                 , "RUN"
                                 ]
