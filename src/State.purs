module State where

import Prelude

type Position = Int

type State =
  { instruction :: Int
  , terminated  :: Boolean
  , robot       :: Position
  , parachute   :: Position }
