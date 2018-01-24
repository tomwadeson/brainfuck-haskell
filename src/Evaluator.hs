{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Evaluator
  ( evaluate
  ) where

import           Control.Monad.Loops (whileM_)
import           Control.Monad.State
import           Data.Char           (chr)
import           Data.Foldable       (traverse_)
import           Types

evaluate :: (MonadConsole m, MonadState Machine m) => Program -> m ()
evaluate = traverse_ evaluate' . unProgram

evaluate' :: (MonadConsole m, MonadState Machine m) => Instruction -> m ()
evaluate' IncrementPointer                  = incrementPointer
evaluate' DecrementPointer                  = decrementPointer
evaluate' IncrementCurrentRegister          = incrementCurrentRegister
evaluate' DecrementCurrentRegister          = decrementCurrentRegister
evaluate' OutputCurrentRegister             = outputCurrentRegister
evaluate' ReadAndSetCurrentRegister         = readAndSetCurrentRegister
evaluate' (DoWhileCurrentRegisterNonZero p) = doWhileCurrentRegisterNonZero p

incrementPointer :: (MonadState Machine m) => m ()
incrementPointer = modify (modifyPointer succ)

decrementPointer :: (MonadState Machine m) => m ()
decrementPointer = modify (modifyPointer pred)

incrementCurrentRegister :: (MonadState Machine m) => m ()
incrementCurrentRegister = modify (modifyCurrentRegister succ)

decrementCurrentRegister :: (MonadState Machine m) => m ()
decrementCurrentRegister = modify (modifyCurrentRegister pred)

outputCurrentRegister :: (MonadConsole m, MonadState Machine m) => m ()
outputCurrentRegister = gets (chr . unRegister . currentRegister) >>= writeChar

readAndSetCurrentRegister :: (MonadConsole m, MonadState Machine m) => m ()
readAndSetCurrentRegister = readInt >>= \r -> modify (modifyCurrentRegister (const (Register r)))

doWhileCurrentRegisterNonZero :: (MonadConsole m, MonadState Machine m) => Program -> m ()
doWhileCurrentRegisterNonZero p = whileM_ currentRegisterNonZero (evaluate p)
  where
    currentRegisterNonZero = gets ((/= 0) . unRegister . currentRegister)
