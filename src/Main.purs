module Main where

import Prelude

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Trans (lift)
import Control.Monad.Eff.Class (liftEff)

import Data.Array as A
import Data.Either
import Data.Int
import Data.List
import Data.Maybe
import Data.Nullable (toMaybe)
import Partial.Unsafe (unsafePartial)

import Text.Parsing.StringParser (ParseError(..))
import Unsafe.Coerce (unsafeCoerce)

import Thermite as T

import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM

import DOM as DOM
import DOM.HTML as DOM
import DOM.HTML.Types as DOM
import DOM.HTML.Window as DOM
import DOM.Node.ParentNode as DOM

import State
import Language.Robo.Spec
import Language.Robo.Parser
import Language.Robo.Interpreter

-- | Maximum distance of the initial position from the origin
maxRange :: Int
maxRange = 5

-- | Initial state of a robot for a given starting position
initial :: Position -> Robot
initial pos =
  { position: pos
  , parachute: pos
  , instruction: 0 }

initialCode :: String
initialCode = """# Useless example program
start: left
       goto start"""

initialState :: State
initialState =
  { r1: initial (-5)
  , r2: initial 5
  , code: initialCode
  , parsed: false
  , program: Right Nil
  , running: false
  , collision: false }

-- | UI actions
data Action = SetCode String
            | Parse
            | Step
            | ToggleRunning
            | Randomize

-- | Font awesome shortcut
fa :: String -> R.ReactElement
fa str = R.i [ RP.className ("fa fa-" <> str) ] []

foreign import data TIMER :: !
foreign import toggleInterval :: forall eff. Eff (timer :: TIMER | eff) Unit

-- | Render UI component
render :: T.Render State _ Action
render dispatch _ state _ =
  [ R.div [ RP.className "canvas" ]
    if state.collision
      then
        [ R.div [ RP.className "collision" ] [ fa "trophy", R.text " Collision!" ] ]
      else
        [ R.div [ RP.className "robot1"
                , RP.style {left: show (screenPos state.r1.position) <> "px", position: "absolute"} ] []
        , R.div [ RP.className "robot2"
                , RP.style {left: show (screenPos state.r2.position) <> "px", position: "absolute"} ] []
        , R.div [ RP.className "parachute1"
                , RP.style {left: show (screenPos state.r1.parachute) <> "px"} ] []
        , R.div [ RP.className "parachute2"
                , RP.style {left: show (screenPos state.r2.parachute) <> "px"} ] []
        ]
  , R.div [ RP.className "panel" ]
      [ R.div [ RP.className "editor" ]
          [ R.textarea
            [ RP.autoFocus true
            , RP.rows "10"
            , RP.spellCheck "false"
            , RP.placeholder "Enter your code here..."
            , RP.onChange \e -> dispatch (SetCode (unsafeCoerce e).target.value) ]
            [ R.text initialCode ]
          ]
          , R.div [ RP.className "program" ]
          [ R.table' [ R.tbody' (programTable state.program state.r1.instruction state.r2.instruction) ] ]
      ]
  , R.div [ RP.className "buttons" ]
    [ R.button [ RP.onClick \_ -> dispatch Parse
               , RP.title "Transfer your program to the robots"
               , RP.disabled state.running ]
               [ fa "cogs", R.text " Parse" ]
    , R.button [ RP.disabled (not state.parsed || state.collision)
               , RP._id "step"
               , RP.title (if (not state.parsed)
                             then "You need to parse your program first"
                             else "Run a single instruction for each robot")
               , RP.onClick \_ -> dispatch Step ]
               [ fa "step-forward", R.text " Step" ]
    , R.button [ RP.disabled (not state.parsed || state.collision)
               , RP.title (if (not state.parsed)
                             then "You need to parse your program first"
                             else (if state.running
                                     then "Stop the running program"
                                     else "Start the program"))
               , RP.onClick \_ -> dispatch ToggleRunning ]
               if state.running
                  then [ fa "stop", R.text " Stop" ]
                  else [ fa "play", R.text " Run" ]
    , R.button [ RP.onClick \_ -> dispatch Randomize
               , RP.title "Deploy the robots to new starting positions"
               , RP.disabled state.running ]
               [ fa "random", R.text " Randomize" ]
    ]
  ]
  where screenPos :: Position -> Number
        screenPos p = 400.0 + 10.0 * toNumber p

-- | Helper function that maps a function over an array and its indices
mapIndexed :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapIndexed f xs = A.zipWith f (A.range 0 (A.length xs)) xs

-- | Render a single instruction as "code"
showHTML :: Instruction -> R.ReactElement
showHTML MoveLeft     = R.code' [ R.text "left" ]
showHTML MoveRight    = R.code' [ R.text "right" ]
showHTML SkipNext     = R.code' [ R.text "skipNext" ]
showHTML (Goto label) = R.code' [ R.text "goto ", R.i' [ R.text label ] ]

-- | Display the program as a table
programTable :: Either ParseError Program -> Int -> Int -> Array R.ReactElement
programTable (Left (ParseError msg)) _ _ = [ R.tr' [ R.td' [ R.text ("Parse error: " <> msg) ] ] ]
programTable (Right Nil) _ _ = []
programTable (Right program) i1 i2 = header `A.cons` mapIndexed row (A.fromFoldable program)
  where header =
          R.tr'
            [ R.th' [ R.text "R", R.sub' [ R.text "1" ] ]
            , R.th' [ R.text "R", R.sub' [ R.text "2" ] ]
            , R.th' [ R.text "Label" ]
            , R.th' [ R.text "Instruction" ] ]
        row i (LInstruction label inst) =
          R.tr'
            [ R.td' [ R.div [ RP.className (if i == i1 then "label1" else "") ] [] ]
            , R.td' [ R.div [ RP.className (if i == i2 then "label2" else "") ] [] ]
            , R.td' [ R.code' [ R.text (fromMaybe "" label) ] ]
            , R.td' [ showHTML inst ]
            ]
        row _ (Comment _) = R.tr' [] -- unreachable case

-- | Update the state
performAction :: forall p. T.PerformAction (timer :: TIMER, random :: RANDOM) State p Action
performAction (SetCode code) _ state = void do
  when state.running (lift (liftEff toggleInterval))
  T.cotransform $
    \_ -> state { code = code
                , parsed = false
                , program = Right Nil
                , running = false }
performAction Randomize _ _ = do
  p1 <- lift $ liftEff $ randomInt 0 (2 * maxRange)
  delta <- lift $ liftEff $ randomInt 1 (2 * maxRange - 1)
  let p2 = (p1 + delta) `mod` (2 * maxRange)
  void $ T.cotransform $ \state -> state { r1 = initial (p1 - maxRange)
                                         , r2 = initial (p2 - maxRange)
                                         , collision = false }
performAction Parse _ state = void $ T.cotransform $ \_ ->
  state { parsed = nonEmpty program
        , program = program
        , r1 = state.r1 { instruction = 0, position = state.r1.parachute }
        , r2 = state.r2 { instruction = 0, position = state.r2.parachute }
        , collision = false
        }
  where program = stripComments <$> parseRobo state.code
        nonEmpty (Right (Cons _ _)) = true
        nonEmpty _                  = false
performAction Step _ state = do
  let st' = either (const state) (step state) state.program
  if st'.collision && st'.running
    then void do
      lift (liftEff toggleInterval)
      T.cotransform $ \_ -> st' { running = false }
    else
      void $ T.cotransform $ \_ -> st'
performAction ToggleRunning _ _ = void do
  lift (liftEff toggleInterval)
  T.cotransform $ \state -> state { running = not state.running }

-- | React spec
spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

-- | Attach the React component to the DOM
main :: Eff (dom :: DOM.DOM) Unit
main = void do
  let component = T.createClass spec initialState
  document <- DOM.window >>= DOM.document
  container <- unsafePartial (fromJust <<< toMaybe <$> DOM.querySelector "#component" (DOM.htmlDocumentToParentNode document))
  RDOM.render (R.createFactory component {}) container
