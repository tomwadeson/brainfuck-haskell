{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RecordWildCards            #-}

module Types where

import           Control.Monad.State
import           Data.Char           (chr)
import qualified Data.Vector         as V

class Monad m =>
      MonadConsole m where
  readInt :: m Int
  writeChar :: Char -> m ()

newtype Brainfuck a = Brainfuck
  { runBrainfuck :: StateT Machine IO a
  } deriving (Functor, Applicative, Monad, MonadState Machine, MonadIO)

instance MonadConsole Brainfuck where
  readInt :: Brainfuck Int
  readInt = liftIO readLn
  writeChar :: Char -> Brainfuck ()
  writeChar = liftIO . putChar

data Instruction
  = IncrementPointer
  | DecrementPointer
  | IncrementCurrentRegister
  | DecrementCurrentRegister
  | OutputCurrentRegister
  | ReadAndSetCurrentRegister
  | DoWhileCurrentRegisterNonZero Program
  deriving (Show)

newtype Program = Program
  { unProgram :: [Instruction]
  } deriving (Show)

data Machine = Machine
  { registers :: V.Vector Register
  , pointer   :: Pointer
  } deriving (Show)

mkMachine :: Int -> Machine
mkMachine n =
  let rs = V.replicate n (Register 0)
  in Machine rs (Pointer 0)

modifyPointer :: (Pointer -> Pointer) -> Machine -> Machine
modifyPointer f m@Machine {..} = m {pointer = f pointer}

currentRegister :: Machine -> Register
currentRegister m = registers m V.! index
  where
    index = unPointer . pointer $ m

modifyCurrentRegister :: (Register -> Register) -> Machine -> Machine
modifyCurrentRegister f m@Machine {..} = m {registers = registers'}
  where
    r = f . currentRegister $ m
    index = unPointer pointer
    registers' = V.update registers [(index, r)]

newtype Register = Register
  { unRegister :: Int
  } deriving (Show, Enum)

newtype Pointer = Pointer
  { unPointer :: Int
  } deriving (Show, Enum)
