module Model exposing (..)

import List.Extra exposing (andThen)
import Dict exposing (fromList)

import Types exposing (..)
import Utilities exposing (..)
import List.Extra exposing (andThen)


emptyModel : Model
emptyModel = Model emptyBoard emptyPieces Nothing Nothing Nothing 0 500 False


emptyBoard : Board
emptyBoard =
  let 
    b = fromList <| [0..h] `andThen` \x -> [0..w] `andThen` \y -> [(x => y => Nothing)]
    -- f = mapGround b
    -- debug = Debug.log "f is" f 
  in b --f


emptyPieces : Board
emptyPieces = fromList []


mapGround : Board -> Board
mapGround board =
  Dict.map (\((r, c) as pos) v ->
    if r < 0 then Just (pos, [(0,0)])
    else v
  ) board


tetriminos : List Tetrimino
tetriminos = [i,l,j,s,z,o,t] 


willItCollide : Board -> Block -> Bool
willItCollide board (gXY, points) =
  -- if gX < 0 || gX > w || gY < 0 then True
  -- else if List.map () 
  let 
    globals = List.map (\lXY -> localToGlobalXY lXY gXY) points
  in
    List.any (\(r, c) ->
     r < 1
    ) globals
  


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