module Utilities exposing (..)

import Collage exposing (rect, filled, Form, Shape)
import Color exposing (..)
import Dict
import Text

import Types exposing (..)


collidesWithWalls : Block -> Bool
collidesWithWalls (gRC, points) =
  let 
    globals = List.map (\lRC -> localToGlobalCoords lRC gRC) points
  in List.any (\(r, c) -> 
    c == 0 || c > w
  ) globals 


collidesWithGround : Block -> Bool
collidesWithGround (gRC, points) =
  let 
    globals = List.map (\lRC -> localToGlobalCoords lRC gRC) points
  in List.any (\(r, c) ->  r > h) globals


collidesWithPieces : Board -> Block -> Bool
collidesWithPieces pieces (gRC, points) =  
  let 
    globals = List.map (\lRC -> localToGlobalCoords lRC gRC) points
  in List.any (\g -> Dict.member g pieces) globals


localToGlobalCoords : Position -> Position -> Position
localToGlobalCoords (lR, lC) (gR, gC) = (lR+gR, lC+gC)


w = 10 -- + 2 -- 2 extra for borders


h = 22 -- + 2 -- 2 extra for borders


gS = 1


rotateR (x, y) = (-y, x)


rotateL (x, y) = (y, -x)


(=>) : a -> b -> (a , b)
(=>) = (,)


getRow : Int -> Int
getRow idx = idx // w
    
    
getCol : Int -> Int
getCol idx = idx // h


square : Shape
square = rect gS gS


shape : Color.Color -> Form
shape color = square |> (filled <| color)