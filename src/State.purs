module State where

import Prelude
import Data.Either
import Text.Parsing.StringParser (ParseError(..))

import Language.Robo.Spec

type Position = Int

type Robot =
  { instruction :: Int
  , terminated  :: Boolean
  , position    :: Position
  , parachute   :: Position }

type State =
  { r1      :: Robot
  , r2      :: Robot
  , code    :: String
  , program :: Either ParseError Program }
