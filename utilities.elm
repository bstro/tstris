module Utilities exposing (..)

import Types exposing (..)

w : Int
w = 10

h : Int
h = 22

gS = 10


(=>) : a -> b -> (a , b)
(=>) = (,)


getRow : Int -> Int
getRow idx = idx // w
    
    
getCol : Int -> Int
getCol idx = idx // h


rotatePiece : Tetrimino -> Tetrimino
rotatePiece = List.map rotate


rotate (x, y) = (-y, x)