{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module AOC014
  ( solve014
  ) where

import Prelude hiding (filter)
import Control.Lens (view, (&), (*~), makeLenses)
import Data.Foldable (foldl')
import Data.Functor.Identity (Identity)
import Data.Map (Map, fromList, keys, mapWithKey, filterWithKey, unionWith, filter, (!))
import Text.Parsec (Stream, Parsec, runParser)
import Text.Parsec.Char (noneOf, digit, string)
import Text.Parsec.Combinator (many1, sepBy1)
import Text.Parsec.Prim (many)

type Parser a = Parsec String () a

data ReactMaterial = ReactMaterial { _matCount :: Int
                                   , _matName :: String
                                   }
  deriving (Show, Eq)
makeLenses ''ReactMaterial
data Reaction = Reaction { _reactIn :: [ReactMaterial]
                         , _reactOut :: ReactMaterial
                         }
  deriving (Show, Eq)
makeLenses ''Reaction

integer :: Parser Int
integer = do
  digits <- many1 digit
  return $ read digits

rm :: Parser ReactMaterial
rm = do
  n <- integer
  string " "
  o <- many1 (noneOf ", ")
  return $ ReactMaterial n o

react :: Parser Reaction
react = do
  components <- rm `sepBy1` string ", "
  string " => "
  out <- rm
  return $ Reaction components out

ingredients :: [Reaction] -> Map String Reaction
ingredients = fromList . fmap (\r@(Reaction ms (ReactMaterial _ o)) -> (o, r))

cost :: Map String Reaction -> Map String Int -> Int
cost rs ms =
  case filter (>0) ms of
       [("ORE", n)] -> n
       needsRunning -> cost rs ms'
         where runReaction o n = rawOuts & reactIn . traverse . matCount *~ numReacts
                                                           & reactOut . matCount *~ numReacts
                 where rawOuts = rs ! o
                       numReacts = (n + outnum - 1) `div` outnum
                       outnum = view (reactOut . matCount) rawOuts
               outs = mapWithKey runReaction $ filterWithKey (\k _ -> k /= "ORE") needsRunning
               removed = snd . r2m . view reactOut <$> outs
               r2m (ReactMaterial n s) = (s, n)
               added = foldl' (unionWith (+)) [] $ fromList . fmap r2m . view reactIn <$> outs
               ms' = unionWith (-) (unionWith (+) ms added) removed

lowerbound :: Int -> Int -> Map String Reaction -> Int
lowerbound supply n rs =
  if cost rs [("FUEL", 2 * n)] > supply then n
  else lowerbound supply (2 * n) rs

fuelMax :: Int -> Int -> Int -> Map String Reaction -> Int
fuelMax _ n 0 _ = n
fuelMax supply n x rs =
  if cost rs [("FUEL", n + x)] > supply then
    fuelMax supply n (x `div` 2) rs
  else
    fuelMax supply (n + x) x rs

solve014 :: String -> Bool -> String
solve014 i b =
  case sequence (runParser react () "014" <$> lines i) of
       Left e -> show e
       Right m -> show $
         if b then fuelMax supply lb lb rs
         else cost rs [("FUEL",1)]
         where rs = ingredients m
               lb = lowerbound supply 1 rs
               supply = 1000000000000
