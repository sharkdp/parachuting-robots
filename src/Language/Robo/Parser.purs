module Language.Robo.Parser
  ( parseRobo
  ) where

import Prelude

import Control.Alt
import Control.Apply

import Data.Char as C
import Data.Either
import Data.Foldable
import Data.List (List())
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
      (string "left" *> pure MoveLeft)
  <|> (string "right" *> pure MoveRight)
  <|> (string "skipNext" *> pure SkipNext)
  <|> (string "goto" *> many1 space *> (Goto <$> parseGotoLabel))
  <?> "Unknown instruction"

toString :: List Char -> String
toString = trim <<< foldMap singleton

parseLabel :: Parser Label
parseLabel = do
  label <- many1 (satisfy (\c -> c /= ':' && c /= '\n' && c /= '\r'))
  char ':'
  skipWhite
  pure $ toString label

parseGotoLabel :: Parser Label
parseGotoLabel = do
  label <- many1 (satisfy (\c -> c /= ' ' && c /= '\n' && c /= '\r'))
  pure $ toString label

parseComment :: Parser LInstruction
parseComment = do
  char '#'
  comment <- many1 (satisfy (\c -> c /= '\n' && c /= '\r'))
  pure $ Comment (toString comment)

parseLInstruction :: Parser LInstruction
parseLInstruction =
  parseComment
  <|> do
    label <- optionMaybe (try parseLabel)
    instr <- parseInstruction
    pure $ LInstruction label instr

separator :: Parser Unit
separator = void (skipWhite *> newline *> many (newline <|> space))

parseProgram :: Parser Program
parseProgram = sepBy parseLInstruction separator <* eof

parseRobo :: String -> Either ParseError Program
parseRobo = runParser parseProgram <<< trim
