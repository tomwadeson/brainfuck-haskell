{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Monad.State
import           Data.Attoparsec.Text
import           Data.Either          (fromRight)
import           Data.Text
import           Evaluator
import           Parser
import           Types

sourcecode :: Text
sourcecode =
  "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

p :: Program
p = fromRight (Program []) $ parseOnly program sourcecode

b :: Brainfuck ()
b = evaluate p

result :: IO ((), Machine)
result = runStateT (runBrainfuck b) (mkMachine 100)

main :: IO ()
main = do
  _ <- result
  pure ()
