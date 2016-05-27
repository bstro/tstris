module Model exposing (..)

import List.Extra exposing (andThen)
import Dict exposing (fromList)

import Types exposing (..)
import Utilities exposing (..)
import List.Extra exposing (andThen)


emptyModel : Model
emptyModel = Model emptyBoard Nothing Nothing Nothing 0 500 False


emptyBoard : Board
emptyBoard =
  let 
    b = fromList <| [0..h] `andThen` \x -> [0..w] `andThen` \y -> [(x => y => Nothing)]
    debug = Debug.log "b is" b 
  in b

tetriminos : List Tetrimino
tetriminos = [i,l,j,s,z,o,t] 


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
    (-1,-1) ,  (0,-1) ,
    (-1, 0) ,  (0, 0)
  ]


t: Tetrimino
t =
  [
             (0,-1) ,
   (-1, 0) , (0, 0) , (1, 0)
  ]