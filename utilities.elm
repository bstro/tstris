module Utilities exposing (..)

import Collage exposing (rect, filled, Form, Shape)
import Color exposing (..)

import Types exposing (..)

w : Int
w = 10

h : Int
h = 22

gS = 10


rotate (x, y) = (-y, x)


(=>) : a -> b -> (a , b)
(=>) = (,)


getRow : Int -> Int
getRow idx = idx // w
    
    
getCol : Int -> Int
getCol idx = idx // h


rotatePiece : Tetrimino -> Tetrimino
rotatePiece = List.map rotate


square : Shape
square = rect gS gS


shape : Color.Color -> Form
shape color = square |> (filled <| color)