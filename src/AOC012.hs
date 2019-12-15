{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module AOC012
  ( solve012
  ) where

import Control.Lens (makeLenses, at, (^.), (&), (+~))
import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.HashSet (HashSet, fromList, member, insert)
import GHC.Generics (Generic)
import Text.Parsec (Stream, Parsec, runParser)
import Text.Parsec.Char (noneOf, digit, string)
import Text.Parsec.Combinator (many1, sepBy1)
import Text.Parsec.Prim (many)

type Parser a = Parsec String () a

data Moon = Moon { _moonX :: Int
                 , _moonY :: Int
                 , _moonZ :: Int
                 , _velX  :: Int
                 , _velY  :: Int
                 , _velZ  :: Int
                 }
  deriving (Show, Eq, Generic)
makeLenses ''Moon

moon :: Parser Moon
moon = do
  let pos = read <$> many1 (noneOf ",>")
  string "<x="
  x <- pos
  string ", y="
  y <- pos
  string ", z="
  z <- pos
  string ">"
  return $ Moon x y z 0 0 0

solve012 :: String -> Bool -> String
solve012 i b =
  case sequence (runParser moon () "012" <$> lines i) of
       Left e -> show e
       Right m ->
         if b then show $ foldl' lcm 1 [px, py, pz]
         else show . sum . fmap energy $ poslist !! 1000
         where gravity (Moon x1 y1 z1 _ _ _) (Moon x2 y2 z2 _ _ _) = (x1 `g` x2, y1 `g` y2, z1 `g` z2)
               g a b = fromEnum (b `compare` a) - 1
               applyg m (dx, dy, dz) = m & velX +~ dx
                                         & velY +~ dy
                                         & velZ +~ dz
               applyv m@(Moon _ _ _ vx vy vz) = m & moonX +~ vx
                                                  & moonY +~ vy
                                                  & moonZ +~ vz
               step ms = fmap (\m -> applyv $ foldl' applyg m (gravities m ms)) ms
               gravities m = fmap (gravity m)
               poslist = iterate step m
               energy (Moon x y z vx vy vz) = (abs x + abs y + abs z) * (abs vx + abs vy + abs vz)
               periodby c v = findperiod (\m -> (m ^. c, m ^. v)) poslist (fromList [])
               px = periodby moonX velX
               py = periodby moonY velY
               pz = periodby moonZ velZ

findperiod :: (Eq b, Hashable b) => (a -> b) -> [[a]] -> HashSet [b] -> Int
findperiod _ [] _ = undefined
findperiod f (x:xs) t = let y = f <$> x in if y `member` t then 0
                                      else 1 + findperiod f xs (y `insert` t)
