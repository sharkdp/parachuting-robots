module Language.Robo.Parser
  ( parseRobo
  ) where

import Prelude

import Control.Alt
import Control.Apply

import qualified Data.Char as C
import Data.Either
import Data.Foldable
import Data.Int (fromString)
import Data.List (List(..), toList)
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.String

import Text.Parsing.StringParser
import Text.Parsing.StringParser.Combinators
import Text.Parsing.StringParser.String

import Language.Robo.Spec

space :: Parser Unit
space = void (string " " <|> string "\t")

skipWhite :: Parser Unit
skipWhite = void (many space)

newline :: Parser Unit
newline = void (char '\n' <|> char '\r')

parseInstruction :: Parser Instruction
parseInstruction =
      (string "left" *> return MoveLeft)
  <|> (string "right" *> return MoveRight)
  <|> (string "skipNext" *> return SkipNext)
  <|> (string "goto" *> many1 space *> (Goto <$> parseGotoLabel))
  <?> "Unknown instruction"

parseLabel :: Parser Label
parseLabel = do
  label <- many1 (satisfy (\c -> c /= ':' && c /= '\n' && c /= '\r'))
  char ':'
  skipWhite
  return $ trim (foldMap C.toString label)

parseGotoLabel :: Parser Label
parseGotoLabel = do
  label <- many1 (satisfy (\c -> c /= ' ' && c /= '\n' && c /= '\r'))
  return $ trim (foldMap C.toString label)

parseLInstruction :: Parser LInstruction
parseLInstruction = do
  label <- optionMaybe (try parseLabel)
  instr <- parseInstruction
  return $ LInstruction label instr

separator :: Parser Unit
separator = void (skipWhite *> newline *> many (newline <|> space))

parseProgram :: Parser Program
parseProgram = sepBy parseLInstruction separator <* eof

parseRobo :: String -> Either ParseError Program
parseRobo = runParser parseProgram <<< trim
