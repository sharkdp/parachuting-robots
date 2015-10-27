module Language.Robo.Spec where

import Prelude
import Data.List
import Data.Maybe
import Data.Generic

type Label = String

data Instruction = MoveLeft
                 | MoveRight
                 | SkipUnlessParachute
                 | Goto Label

data LInstruction = LInstruction (Maybe Label) Instruction

type Program = List LInstruction

derive instance genericInstruction :: Generic Instruction
derive instance genericLInstruction :: Generic LInstruction

instance showInstruction :: Show Instruction where show = gShow
instance showLInstruction :: Show LInstruction where show = gShow
