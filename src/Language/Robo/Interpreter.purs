module Language.Robo.Interpreter
  ( step
  ) where

import Prelude

import Data.List
import Data.Maybe

import Language.Robo.Spec
import State

-- | Run a single instruction of the program for both robots
step :: State -> Program -> State
step st program =
  st { r1 = r1'
     , r2 = r2'
     , collision = compare st.r1.position st.r2.position /= compare r1'.position r2'.position
   }

  where
    r1' = stepRobot st.r1
    r2' = stepRobot st.r2

    stepRobot robo =
      case (index program robo.instruction) of
        Just (LInstruction _ instr) -> eval instr
        _ -> robo

      where
        eval MoveRight =
          robo { position = robo.position + 1 , instruction = robo.instruction + 1 }
        eval MoveLeft =
          robo { position = robo.position - 1 , instruction = robo.instruction + 1 }
        eval SkipNext =
          robo { instruction = if onParachute robo.position
                                 then robo.instruction + 2
                                 else robo.instruction + 1 }
        eval (Goto label) =
          robo { instruction = findInstruction label }

        onParachute pos = pos == st.r1.parachute || pos == st.r2.parachute

        -- Jump to instruction -1 if label cannot be found
        findInstruction label =
          fromMaybe (-1) $ findIndex (\(LInstruction mlabel _) -> mlabel == Just label) program
