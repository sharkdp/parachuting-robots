module Language.Robo.Parser
  ( parseRobo
  ) where

import Prelude

import qualified Data.Char as C
import Data.Either
import Data.String
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.List (List(..), toList)
import Data.Int (fromString)
import Data.Foldable

import Control.Alt
import Control.Apply

import Control.Monad.Eff

import Text.Parsing.StringParser
import Text.Parsing.StringParser.String
import Text.Parsing.StringParser.Combinators

import Language.Robo.Spec

whitespace :: Parser String
whitespace = string " " <|> string "\n" <|> string "\t" <|> string "\r"

sepWhite :: Parser Unit
sepWhite = many1 whitespace *> return unit

parseInstruction :: Parser Instruction
parseInstruction =
  (string "left" *> return MoveLeft)
  <|>
  (string "right" *> return MoveRight)
  <|>
  (string "skip" *> return SkipIfParachute)
  <|>
  (string "goto" *> sepWhite *> (Goto <$> parseGotoLabel))
  <?> "Unknown instruction"

parseLabel :: Parser Label
parseLabel = try $ do
  label <- many1 (satisfy (\c -> c /= ':' && c /= '\n'))
  char ':'
  many (char ' ')
  return $ trim $ foldMap C.toString label

parseGotoLabel :: Parser Label
parseGotoLabel = do
  label <- many1 (satisfy (/= '\n'))
  return $ trim $ foldMap C.toString label

parseLInstruction :: Parser LInstruction
parseLInstruction = do
  label <- optionMaybe parseLabel
  ins <- parseInstruction
  return $ LInstruction label ins

newlines :: Parser Unit
newlines = void $ many1 newline
  where newline = char '\n' <|> char '\r'

parseProgram :: Parser Program
parseProgram = sepBy parseLInstruction newlines <* eof

parseRobo :: String -> Either ParseError Program
parseRobo = runParser parseProgram <<< trim
