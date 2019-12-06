module AOC006 
  ( solve006
  ) where

import Control.Lens (ix, (^?!))
import Data.HashMap.Strict hiding (filter)
import Data.List.Split (splitOn)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, mconcat, (<>))

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

type OrbitMap = HM.HashMap String String
type Graph = HM.HashMap String (HS.HashSet String)

solve006 :: String -> Bool -> Integer
solve006 i b =
  let o = orbits <$> lines i
      ot = HM.fromList o
      vs = nub $ (fst <$> o) ++ (snd <$> o)
      g = HM.fromList $ (\v -> (v, matches v o)) <$> vs
  in  if b then bfs (HS.fromList ["YOU"]) "SAN" g - 2
      else sum . fmap (olen ot) $ HM.keys ot

olen :: OrbitMap -> String -> Integer
olen _ "COM" = 0
olen o x = 1 + olen o (o ^?! ix x)

orbits :: String -> (String, String)
orbits x = let [a, b] = splitOn ")" x in (b, a)

matches :: String -> [(String, String)] -> HS.HashSet String
matches v o = HS.fromList $ l ++ r
  where l = snd <$> filter ((==v) . fst) o
        r = fst <$> filter ((==v) . snd) o

bfs :: HS.HashSet String -> String -> Graph -> Integer
bfs s e g = if e `HS.member` s then 0
            else 1 + bfs t e h
  where t = s <> mconcat (fromMaybe mempty . flip HM.lookup g <$> HS.toList s)
        h = g `HM.difference` HM.fromList (zip (HS.toList s) (repeat ()))
