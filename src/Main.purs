module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Random

import Data.Either
import Data.List
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)
import Unsafe.Coerce (unsafeCoerce)

import qualified Thermite as T

import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Document as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.ParentNode as DOM
import qualified DOM.Node.Types as DOM

import State
import Language.Robo.Parser
import Language.Robo.Interpreter

initial :: Position -> Robot
initial pos =
  { instruction: 0
  , terminated: false
  , position: pos
  , parachute: pos }

initialState :: State
initialState =
  { r1: initial (-3)
  , r2: initial 4
  , code: ""
  , program: Right Nil }

data Action = SetCode String
            | Initialize
            | Parse
            | Step

fa :: String -> _
fa str = R.i [ RP.className ("fa fa-" ++ str) ] []

render :: T.Render State _ Action
render dispatch _ state _ =
  [ R.p' [ R.text (show state.r1.position) ]
  , R.p' [ R.text (show state.r2.position) ]
  , R.p' [ R.text (show state.code) ]
  , R.textarea
      [ RP.autoFocus true
      , RP.rows "10"
      , RP.spellCheck "false"
      , RP.onChange \e -> dispatch (SetCode (unsafeCoerce e).target.value) ]
      [ R.text "" ]
  , R.div [ RP.className "buttons" ]
    [ R.button [ RP.onClick \_ -> dispatch Initialize ] [ fa "random", R.text " Initialize" ]
    , R.button [ RP.onClick \_ -> dispatch Parse ] [ fa "paragraph", R.text " Parse" ]
    , R.button [ RP.onClick \_ -> dispatch Step ] [ fa "step-forward", R.text " Step" ]
    ]
  ]

performAction :: T.PerformAction _ State _ Action
performAction (SetCode code) _ state update = update $
  state { code = code }
performAction Initialize _ state update = do
  p1 <- randomInt (-10) 10
  p2 <- randomInt (-10) 10
  update $ state { r1 = initial p1, r2 = initial p2 }
performAction Parse _ state update = update $
  state { program = parseRobo state.code
        , r1 = state.r1 { instruction = 0 }
        , r2 = state.r2 { instruction = 0 }
        }
performAction Step _ state update = update $
  state { r1 = either (const state.r1) (flip step state.r1) state.program }

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

main = void do
  let component = T.createClass spec initialState
  document <- DOM.window >>= DOM.document
  container <- fromJust <<< toMaybe <$> DOM.querySelector "#container" (DOM.htmlDocumentToParentNode document)
  R.render (R.createFactory component {}) container
