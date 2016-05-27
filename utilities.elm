module Utilities exposing (..)

import Collage exposing (rect, filled, Form, Shape)
import Color exposing (..)
import Text

import Types exposing (..)

type1 =
  { typeface = [ "Courier" ]
  , height   = Just 6
  , color    = black
  , bold     = True
  , italic   = False
  , line     = Just Text.Under
  }

w = 10 -- + 2 -- 2 extra for borders

h = 22 -- + 2 -- 2 extra for borders

gS = 20

rotateR (x, y) = (-y, x)

rotateL (x, y) = (y, -x)

(=>) : a -> b -> (a , b)
(=>) = (,)


getRow : Int -> Int
getRow idx = idx // w
    
    
getCol : Int -> Int
getCol idx = idx // h


-- rotatePiece : Tetrimino -> Tetrimino
-- rotatePiece = List.map rotate


square : Shape
square = rect gS gS


shape : Color.Color -> Form
shape color = square |> (filled <| color)