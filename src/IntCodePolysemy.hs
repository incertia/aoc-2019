{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonadFailDesugaring #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module IntCodePolysemy
  ( TapeFormat
  , TapeMachine
  , toTape
  , execMachine
  , runMachine
  , initialMachine
  ) where

import Control.Lens
  (view, to, (.~), (?~), (+~))
import Control.Lens.At
  (at)
import Control.Lens.TH
  (makeLenses)
import Control.Monad
  (when)
import Data.Function
  ((&))
import Data.Has
  (Has, hasLens)
import Data.Hashable
  (Hashable)
import Data.HashMap.Strict
  (HashMap, fromList)
import Data.Maybe
  (fromMaybe)

import Polysemy
  (Member, Sem, makeSem, run)
import Polysemy.Input
  (Input, input, runInputList)
import Polysemy.Output
  (Output, output, runOutputList)
import Polysemy.State
  (State, get, gets, modify, runState)

type Tape a = HashMap a a

data IntCode = OpAdd
             | OpMul
             | OpInput
             | OpOutput
             | OpJumpT
             | OpJumpF
             | OpLT
             | OpEQ
             | OpSetBase
             | Halt
  deriving (Show, Eq, Bounded)

data Mode = Positional
          | Immediate
          | Relative
  deriving (Show, Eq, Bounded)

data TapeMachine a = TapeMachine { _ip   :: a
                                 , _base :: a
                                 , _tape :: Tape a
                                 }
  deriving (Show, Eq)
makeLenses ''TapeMachine

class (Show a, Num a, Integral a, Ord a, Hashable a) => TapeFormat a
instance TapeFormat Int
instance TapeFormat Integer

toTape :: (Read a, TapeFormat a) => String -> [a]
toTape x = read $ "[" ++ x ++ "]"

decode :: forall a s r
        . ( TapeFormat a
          , Has (TapeMachine a) s
          , Member (State s) r
          )
       => Sem r (IntCode, [a], [a], a)
decode = do
  pc <- gets @s (view $ hasLens . ip)
  t  <- gets @s (view $ hasLens . tape)
  rb <- gets @s (view $ hasLens . base)
  let viewTape i  = view (at i . to (fromMaybe 0)) t
      opc         = viewTape pc
      (op, d)     = toOpcode opc pc
      rawptrs     = viewTape . (pc+) <$> [1..]
      modes       = toMode opc       <$> [1..]
      adjust m p  = if m == Relative then p + rb else p
      viewArg m p = if m == Immediate then p else viewTape p
      ptrs        = zipWith adjust modes rawptrs
      args        = zipWith viewArg modes ptrs
  return (op, ptrs, args, d)

toOpcode :: TapeFormat a => a -> a -> (IntCode, a)
toOpcode op ip =
  case op `mod` 100 of
       1  -> (OpAdd    , 4)
       2  -> (OpMul    , 4)
       3  -> (OpInput  , 2)
       4  -> (OpOutput , 2)
       5  -> (OpJumpT  , 3)
       6  -> (OpJumpF  , 3)
       7  -> (OpLT     , 4)
       8  -> (OpEQ     , 4)
       9  -> (OpSetBase, 2)
       99 -> (Halt     , 1)
       x  -> error $ "bad opcode at position " ++ show ip ++ ": " ++ show x

toMode :: TapeFormat a => a -> Int -> Mode
toMode op n =
  case (op `div` 10^(n + 1)) `mod` 10 of
       0 -> Positional
       1 -> Immediate
       2 -> Relative
       x -> error $ "bad mode (" ++ show op ++ ", " ++ show n ++ "): " ++ show x

execMachine :: forall a s r
             . ( TapeFormat a
               , Has (TapeMachine a) s
               , Member (Input (Maybe a)) r
               , Member (Output a) r
               , Member (State s) r
               )
            => Sem r ()
execMachine = do
  (op, pa:_:pc:_, a:b:_, d) <- decode @a @s
  if op == Halt then return ()
  else do
    modify @s $ hasLens . ip +~ d
    case op of
         OpAdd     -> modify @s $ hasLens . tape . at pc ?~ a + b
         OpMul     -> modify @s $ hasLens . tape . at pc ?~ a * b
         OpInput   -> do
           i <- fromMaybe (error "unexpected end of input") <$> input @(Maybe a)
           modify @s $ hasLens . tape . at pa ?~ i
         OpLT      -> modify @s $ hasLens . tape . at pc ?~ (fromIntegral . fromEnum $ a < b)
         OpEQ      -> modify @s $ hasLens . tape . at pc ?~ (fromIntegral . fromEnum $ a == b)
         OpSetBase -> modify @s $ hasLens . base +~ a
         OpJumpT   -> when (a /= 0) . modify @s $ hasLens . ip .~ b
         OpJumpF   -> when (a == 0) . modify @s $ hasLens . ip .~ b
         _         -> pure ()
    when (op == OpOutput) $ output @a a
    execMachine @a @s

initialMachine :: TapeFormat a => [a] -> TapeMachine a
initialMachine a = TapeMachine 0 0 . fromList $ zip [0..] a

runMachine :: forall a. TapeFormat a => TapeMachine a -> [a] -> [a]
runMachine m i = execMachine @a @(TapeMachine a)
               & runState m
               & runInputList i
               & runOutputList
               & run
               & fst
