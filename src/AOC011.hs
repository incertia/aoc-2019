{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module AOC011
  ( solve011
  ) where

import Control.Lens (view, at, to, (<&>), (&), (^.), (.~), (?~))
import Control.Monad.State (execState)
import Data.Foldable (foldl')
import Data.List (scanl', intercalate)
import Data.List.Split (chunksOf)
import Data.HashMap.Strict (HashMap, keys, fromList)
import Data.Maybe (fromMaybe)

import IntCode

solve011 :: String -> Bool -> String
solve011 i b =
  if b then intercalate "\n" display
  else show . length . keys $ last
  where r = view tapeOut . execState eval $ m & tapeIn .~ inputs
        m = initialMachine [] $ toTape i
        outs = chunksOf 2 r <&> \[x, y] -> (x, y)
        travel ((x, y), (dx, dy)) d =
          let (ndx, ndy) = case d of
                                0 -> (-dy, dx)
                                1 -> (dy, -dx)
          in ((x + ndx, y + ndy), (ndx, ndy))
        inputs = hulls <&> \((p, _), h) -> viewh h p
        hulls = scanl' travelPaint (((0, 0), (0, 1)), initialHull) outs
        travelPaint (pd, h) (col, dir) =
          let npd = travel pd dir
              (np, nd) = npd
              (p, _) = pd
              nh = h & at p ?~ col
          in  (npd, nh)
        initialHull :: HashMap (Int, Int) Int
        initialHull = fromList [] & at (0, 0) ?~ if b then 1 else 0
        last = hulls !! (length hulls - 1) & snd
        positions = keys last
        minX = minimum $ fst <$> positions
        maxX = maximum $ fst <$> positions
        minY = minimum $ snd <$> positions
        maxY = maximum $ snd <$> positions
        display = fmap (showg . viewh last) <$> grid
        viewh h p = h ^. at p . to (fromMaybe 0)
        grid = sequence ((,) <$> [minX..maxX]) <$> [maxY,maxY-1..minY]
        showg = \case { 1 -> '#'; _ -> ' ' }
