module Model exposing (..)

import List exposing (repeat, concat)
import List.Extra exposing (andThen)
import Dict exposing (fromList)

import Types exposing (..)
import Utilities exposing (..)
import List.Extra exposing (andThen)


emptyModel : Model
emptyModel = Model emptyBoard Nothing Nothing 0.0


-- write a function to find number of Nothings in a row
-- if nothings.length == 0 then remove the row
-- if rowsToClear == 1, then pts, if rowsToClear == 2 then pts*2


emptyBoard : Board
emptyBoard =
  let lst = [1..w] `andThen` \x -> [1..h] `andThen` \y -> [(x => y => Nothing)]
  in
  Board (fromList lst) <| Just ((0,0) => t) 


i : Tetrimino
i =
  [
    (-1, 0) , (0, 0) , (1, 0) , (2, 0)
  ]


l : Tetrimino
l =
  [
                      (1,-1) ,
    (-1, 0) , (0, 0), (1, 0)
  ]


j : Tetrimino
j = 
  [
    (-1,-1) ,
    (-1, 0) , (0, 0) , (1, 0) 
  ]


s : Tetrimino
s =
  [
             (0,-1) , (1,-1) ,
    (-1, 0), (0, 0)
  ]


z : Tetrimino
z =
  [
    (-1,-1) , (0,-1)
            , (0, 0) , (1, 0) 
  ]


o : Tetrimino
o =
  [
    (0,-1) , (1,-1) ,
    (0, 0) , (1, 0)
  ]


t: Tetrimino
t =
  [
             (0,-1) ,
   (-1, 0) , (0, 0) , (1, 0)
  ]