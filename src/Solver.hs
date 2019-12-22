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
import AOC007 (solve007)
import AOC008 (solve008)
import AOC009 (solve009)
import AOC010 (solve010)
import AOC011 (solve011)
import AOC012 (solve012)
import AOC013 (solve013)
import AOC014 (solve014)
import AOC015 (solve015)
import AOC016 (solve016)
import AOC017 (solve017)
import AOC019 (solve019)
import AOC021 (solve021)
import AOC022 (solve022)

import AOC009.Polysemy (solve009poly)

solvers :: H.HashMap Integer (String -> Bool -> String)
solvers = H.fromList
  [ (001, solve001), (002, solve002), (003, solve003), (004, solve004), (005, solve005)
  , (006, solve006), (007, solve007), (008, solve008), (009, solve009), (010, solve010)
  , (011, solve011), (012, solve012), (013, solve013), (014, solve014), (015, solve015)
  , (016, solve016), (017, solve017), (019, solve019)
  , (021, solve021), (022, solve022)
  , (1009, solve009poly)
  ]

solve :: Integer -> Bool -> IO ()
solve n b = case H.lookup n solvers of
  Nothing -> putStrLn "no solver implemented"
  Just f  ->
    let ifile = "./input/" ++ pad 3 '0' (show n) in
    putStrLn =<< flip f b . either (const "") id <$> try @IOError (readFile ifile)
  where pad l x xs = if length xs < l then pad l x (x:xs) else xs
