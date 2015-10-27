module State where

import Prelude

import Data.Either
import Text.Parsing.StringParser (ParseError(..))

import Language.Robo.Spec

-- | Robot and parachute positions
type Position = Int

-- | Position of a robot, its parachute and the
-- | state of the interpreter.
type Robot =
  { position    :: Position
  , parachute   :: Position
  , instruction :: Int }

-- | UI state
type State =
  { r1      :: Robot
  , r2      :: Robot
  , code    :: String
  , parsed  :: Boolean
  , program :: Either ParseError Program
  , running :: Boolean
  }
