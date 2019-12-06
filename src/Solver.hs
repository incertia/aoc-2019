{-# LANGUAGE TypeApplications #-}

module Solver (solve) where

import Control.Exception (try)

import qualified Data.HashMap.Strict as H (HashMap, fromList, lookup)

import AOC001 (solve001)
import AOC002 (solve002)
import AOC003 (solve003)
import AOC004 (solve004)
import AOC005 (solve005)
import AOC006 (solve006)

solvers :: H.HashMap Integer (String -> Bool -> Integer)
solvers = H.fromList
  [ (001, solve001), (002, solve002), (003, solve003), (004, solve004), (005, solve005)
  , (006, solve006)
  ]

solve :: Integer -> Bool -> IO ()
solve n b = case H.lookup n solvers of
  Nothing -> putStrLn "no solver implemented"
  Just f  ->
    let ifile = "./input/" ++ pad 3 '0' (show n) in
    print =<< flip f b . either (const "") id <$> try @IOError (readFile ifile)
  where pad n x xs = if length xs < n then pad n x (x:xs) else xs
