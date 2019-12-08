{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonadFailDesugaring #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module IntCode
  ( toTape, eval
  , TapeMachine, initialMachine
  , tapePC, tapeIn, tapeOut, tape
  , readPure, readT, writeT
  ) where

import Control.Lens (makeLenses, at, ix, use, uses, over, _2, (&), (^.), (^?!), (.~), (%~), (?~), (%=), (?=))
import Control.Monad (when)
import Control.Monad.State.Strict (MonadState)
import Data.Has (Has, hasLens)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)

import qualified Data.HashMap.Strict as HM (fromList)

data IntCode = OpAdd | OpMul | OpInput | OpOutput | OpJumpT | OpJumpF | OpLT | OpEQ | Halt
  deriving (Show, Eq)

type Tape = HashMap Int Int

data TapeMachine = TapeMachine { _tapePC      :: Int
                               , _tapeIn      :: [Int]
                               , _tapeInPos   :: Int
                               , _tapeOut     :: [Int]
                               , _tape        :: Tape
                               }
  deriving (Show, Eq)
makeLenses ''TapeMachine

toTape :: String -> Tape
toTape x = let y = read $ "[" ++ x ++ "]" in HM.fromList $ zip [0..] y

initialMachine :: [Int] -> Tape -> TapeMachine
initialMachine i = TapeMachine 0 i 0 []

decode :: (Has TapeMachine s, MonadState s m) => m (IntCode, [Int], [Int], Int)
decode = do
  i <- use (hasLens . tapePC)
  t <- use (hasLens . tape)
  n <- readT i
  (op, ip) <- over _2 (i+) <$>
    case n `mod` 100 of
         1  -> pure (OpAdd, 4)
         2  -> pure (OpMul, 4)
         3  -> pure (OpInput, 2)
         4  -> pure (OpOutput, 2)
         5  -> pure (OpJumpT, 3)
         6  -> pure (OpJumpF, 3)
         7  -> pure (OpLT, 4)
         8  -> pure (OpEQ, 4)
         99 -> pure (Halt, 1)
         x  -> use (hasLens . tapePC) >>= \j -> error $ "Unexpected opcode at index " ++ show j ++ ": " ++ show x
  let modes = mode n <$> [1..]
      ptrs  = flip readPure t . (i+) <$> [1..]
      args  = zipWith (\m x -> if m then readPure x t else x) modes ptrs
  return (op, ptrs, args, ip)

mode :: Int -> Int -> Bool
mode x n =
  case (x `div` 10^(n + 1)) `mod` 10 of
       0 -> True  -- Rel
       1 -> False -- Imm
       z -> error $ "Unexpected mode: (" ++ show x ++ ", " ++ show n ++ ") -> " ++ show z

--input :: (Has TapeMachine s, MonadState s m) => m Int
--input = do
--  p <- use $ hasLens . tapeInPos
--  r <- preuse $ hasLens . tapeIn . ix p
--  hasLens . tapeInPos += 1
--  return $ fromMaybe (error $ "Ran out of input at position " ++ show p) r

output :: (Has TapeMachine s, MonadState s m) => Int -> m ()
output = (hasLens . tapeOut %=) . (:)

readPure :: Int -> Tape -> Int
readPure i t = fromMaybe (error $ "Invalid tape index: " ++ show i) $ t ^. at i

readT :: (Has TapeMachine s, MonadState s m) => Int -> m Int
readT = uses (hasLens . tape) . readPure

writeT :: (Has TapeMachine s, MonadState s m) => Int -> Int -> m ()
writeT i x = hasLens . tape . at i ?= x

eval :: (Has TapeMachine s, MonadState s m) => m ()
eval = do
  (op, pa:_:pc:_, a:b:_, ip) <- decode
  if op == Halt then return ()
  else do
    let modifyState s = s & writeP s
                          & tapeInPos %~ (if op == OpInput then succ else id)
                          & tapePC .~ npc
        writeP s =
          case op of
               OpAdd   -> tape . at pc ?~ a + b
               OpMul   -> tape . at pc ?~ a * b
               OpInput -> tape . at pa ?~ (s ^?! tapeIn . ix (s ^. tapeInPos))
               OpLT    -> tape . at pc ?~ (fromIntegral . fromEnum $ a < b)
               OpEQ    -> tape . at pc ?~ (fromIntegral . fromEnum $ a == b)
               _       -> id
        npc =
          case op of
               OpJumpT -> if a /= 0 then b else ip
               OpJumpF -> if a == 0 then b else ip
               _       -> ip
    hasLens %= modifyState
    eval
    when (op == OpOutput) $ output a
