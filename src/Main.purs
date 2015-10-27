module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Either
import Data.List.Lazy (iterate, take)
import Data.Maybe

import DOM

import Graphics.Canvas (getCanvasElementById, getContext2D, clearRect)
import Graphics.Drawing (render)

import State
import Graphics
import Language.Robo.Parser
import Language.Robo.Interpreter

foreign import onStep :: forall eff. (String -> Eff (dom :: DOM | eff) Unit)
                       -> Eff (dom :: DOM | eff) Unit

foreign import printMessage :: forall eff. String -> Eff (dom :: DOM | eff) Unit

initial :: Position -> State
initial pos =
  { instruction: 0
  , terminated: false
  , robot: pos
  , parachute: pos }

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  onStep $ \code -> do
    case (parseRobo code) of
      Left err -> do
        printMessage $ "Parse error: " ++ show err
      Right program -> do
        clearRect ctx { x: 0.0, y: 0.0, w: 800.0, h: 50.0 }
        {-- printMessage $ "Parsing complete:<br>" ++ show program --}
        let i1 = initial (-3)
        let i2 = initial 5
        render ctx $ renderRobots i1 i2
        let steps = take 100 $ iterate (step program) i1
        printMessage $ show (_.robot <$> steps)
