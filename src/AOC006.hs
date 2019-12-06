{-# LANGUAGE TupleSections #-}

module AOC006 
  ( solve006
  ) where

import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.List.Split (splitOn)
import Data.Monoid (mempty, (<>))
import Data.Tree (Tree(Node), unfoldTree)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

type Graph a b = HM.HashMap a (HS.HashSet b)

solve006 :: String -> Bool -> Integer
solve006 i b =
  let o = orbits <$> lines i
      ot = unfoldTree (\x -> (x,) $ snd <$> filter ((==x) . fst) o) "COM"
      g = HM.fromList . HS.toList . HS.map (\v -> (v, matches v o))
        . HS.fromList $ (fst <$> o) ++ (snd <$> o)
  in  if b then bfs (HS.singleton "YOU") "SAN" g - 2
      else sum . toDepth 0 $ ot

orbits :: String -> (String, String)
orbits x = let [a, b] = splitOn ")" x in (a, b)

toDepth :: Integer -> Tree a -> Tree Integer
toDepth d (Node a ts) = Node d (toDepth (d + 1) <$> ts)

matches :: (Hashable a, Eq a) => a -> [(a, a)] -> HS.HashSet a
matches v o = HS.fromList $ l ++ r
  where l = snd <$> filter ((==v) . fst) o
        r = fst <$> filter ((==v) . snd) o

bfs :: (Hashable a, Eq a) => HS.HashSet a -> a -> Graph a a -> Integer
bfs s e g = if e `HS.member` s then 0
            else 1 + bfs t e h
  where t = foldl' (<>) mempty . HS.map (flip (HM.lookupDefault mempty) g) $ s
        h = HM.filterWithKey (\k _ -> not $ k `HS.member` s) g
