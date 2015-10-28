module Language.Robo.Spec where

import Prelude

import Data.List
import Data.Maybe
import Data.Generic

type Label = String

data Instruction = MoveLeft
                 | MoveRight
                 | SkipNext
                 | Goto Label

data LInstruction = LInstruction (Maybe Label) Instruction

type Program = List LInstruction

derive instance genericInstruction :: Generic Instruction
derive instance genericLInstruction :: Generic LInstruction

instance showInstruction :: Show Instruction where show = gShow
instance eqInstruction :: Eq Instruction where eq = gEq
instance showLInstruction :: Show LInstruction where show = gShow
instance eqLInstruction :: Eq LInstruction where eq = gEq
