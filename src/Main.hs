module Main (main) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Solver (solve)
import System.Environment (getProgName, getArgs)
import Text.Read (readMaybe)

timed :: IO () -> IO ()
timed io = do
  start <- getPOSIXTime
  io
  end <- getPOSIXTime
  putStrLn $ "solution took " ++ show (round ((end - start) * 1000)) ++ "ms"

main :: IO ()
main = do
  pname <- getProgName
  args <- getArgs
  let usage = putStrLn $ "usage: " ++ pname ++ " <problem-number> <A/B>"

  case args of
    [n, b] ->
      case readMaybe n :: Maybe Integer of
           Just n' ->
             case b of
                  "A" -> timed $ solve n' False
                  "B" -> timed $ solve n' True
                  _   -> usage
           Nothing -> usage
    _   -> usage
