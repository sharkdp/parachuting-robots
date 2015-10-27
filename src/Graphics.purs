module Graphics where

import Prelude

import Control.Monad.Eff

import Data.Maybe
import Data.List
import Data.Int
import Data.Monoid

import Graphics.Drawing hiding (red, blue)

import State

size :: Number
size = 10.0

width :: Number
width = 800.0

height :: Number
height = 50.0

hh :: Number
hh = height / 2.0

red  = rgb 255.0  57.0  95.0
blue = rgb  31.0 136.0 178.0

point :: Number -> Number -> Point
point x y = { x: x, y: y }

line :: Number -> Number -> Number -> Number -> Drawing
line x1 y1 x2 y2 =
    outlined (outlineColor black) $ path [ {x: x1, y: y1}, {x: x2, y: y2} ]

{-- part :: Part -> Drawing --}
{-- part (Part c) = --}
{--     shade $ style $ rectangle 0.0 0.0 size size --}
{--       where style =    outlined (outlineColor black <> lineWidth 2.0) --}
{--                     <> filled (fillColor (color c)) --}
{--             shade = shadow (shadowColor black <> shadowOffset 2.0 2.0 <> shadowBlur 10.0) --}

renderRobot :: Position -> Color -> Drawing
renderRobot pos col = style $ circle (width / 2.0 + toNumber pos * size) hh size
  where style = filled (fillColor col)

renderAxis :: Drawing
renderAxis = line 0.0 hh width hh

renderRobots :: State -> State -> Drawing
renderRobots s1 s2 =
  let r1 = s1.robot
      r2 = s2.robot
  in
    renderAxis
    <> renderRobot s1.robot red
    <> renderRobot s2.robot blue
