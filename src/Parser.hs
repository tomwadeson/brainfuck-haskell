module Parser
  ( program
  ) where

import           Data.Attoparsec.Text
import           Types

program :: Parser Program
program = Program <$> many' (skipOthers *> instruction) <* skipOthers
  where
    skipOthers = skipWhile $ notInClass "><+-.,[]"

instruction :: Parser Instruction
instruction =
  choice
    [ incrementPointer
    , decrementPointer
    , incrementCurrentRegister
    , decrementCurrentRegister
    , outputCurrentRegister
    , readAndSetCurrentRegister
    , doWhileCurrentRegisterNonZero
    ]

incrementPointer :: Parser Instruction
incrementPointer = const IncrementPointer <$> char '>'

decrementPointer :: Parser Instruction
decrementPointer = const DecrementPointer <$> char '<'

incrementCurrentRegister :: Parser Instruction
incrementCurrentRegister = const IncrementCurrentRegister <$> char '+'

decrementCurrentRegister :: Parser Instruction
decrementCurrentRegister = const DecrementCurrentRegister <$> char '-'

outputCurrentRegister :: Parser Instruction
outputCurrentRegister = const OutputCurrentRegister <$> char '.'

readAndSetCurrentRegister :: Parser Instruction
readAndSetCurrentRegister = const ReadAndSetCurrentRegister <$> char '.'

doWhileCurrentRegisterNonZero :: Parser Instruction
doWhileCurrentRegisterNonZero =
  DoWhileCurrentRegisterNonZero <$> (char '[' *> program <* char ']')
