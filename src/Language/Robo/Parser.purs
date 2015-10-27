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

space :: Parser String
space = string " " <|> string "\t"

skipWhite :: Parser Unit
skipWhite = many space *> return unit

sepWhite :: Parser Unit
sepWhite = many1 space *> return unit

newline :: Parser Char
newline = char '\n' <|> char '\r'

newlines :: Parser Unit
newlines = void $ many1 newline

parseInstruction :: Parser Instruction
parseInstruction =
  skipWhite *> (
        (string "left" *> return MoveLeft)
    <|> (string "right" *> return MoveRight)
    <|> (string "skipNext" *> return SkipNext)
    <|> (string "goto" *> sepWhite *> (Goto <$> parseGotoLabel))
    <?> "Unknown instruction"
  ) <* skipWhite

parseLabel :: Parser Label
parseLabel = do
  label <- many1 (satisfy (\c -> c /= ':' && c /= '\n' && c /= '\r'))
  char ':'
  skipWhite
  return $ trim $ foldMap C.toString label

parseGotoLabel :: Parser Label
parseGotoLabel = do
  label <- many1 (satisfy (\c -> c /= ' ' && c /= '\n' && c /= '\r'))
  return $ trim $ foldMap C.toString label

parseLInstruction :: Parser LInstruction
parseLInstruction = do
  label <- optionMaybe (try parseLabel)
  instr <- parseInstruction
  return $ LInstruction label instr

parseProgram :: Parser Program
parseProgram = sepBy parseLInstruction newlines <* eof

parseRobo :: String -> Either ParseError Program
parseRobo = runParser parseProgram <<< trim
