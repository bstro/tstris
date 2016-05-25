module Model exposing (..)

import List exposing (repeat, concat)

import Types exposing (..)


emptyModel : Model
emptyModel =
  Model emptyBoard Nothing Nothing Nothing 0.0


emptyBoard : List (Maybe Block)
emptyBoard = concat <| repeat w <| repeat h <| Nothing 


w : Int
w = 10


h : Int
h = 22


gS = 10


i : Block
i =
  [
    (-1, 0) , (0, 0) , (1, 0) , (2, 0)
  ]


l : Block
l =
  [
                      (1,-1) ,
    (-1, 0) , (0, 0), (1, 0)
  ]


j : Block
j = 
  [
    (-1,-1) ,
    (-1, 0) , (0, 0) , (1, 0) 
  ]


s : Block
s =
  [
             (0,-1) , (1,-1) ,
    (-1, 0), (0, 0)
  ]


z : Block
z =
  [
    (-1,-1) , (0,-1)
            , (0, 0) , (1, 0) 
  ]


o : Block
o =
  [
    (0,-1) , (1,-1) ,
    (0, 0) , (1, 0)
  ]


t: Block
t =
  [
             (0,-1) ,
   (-1, 0) , (0, 0) , (1, 0)
  ]