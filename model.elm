module Model exposing (..)

import List.Extra exposing (andThen, getAt)
import Dict exposing (fromList)

import Types exposing (..)
import Utilities exposing (..)
import List.Extra exposing (andThen)


-- left = Left 37
-- up = Up 38
-- right = Right 39
-- down = Down 40
-- space = Space 32


setPiece : Board -> Block -> Board
setPiece board (gRC, t) =
  let 
    globals = List.map (\lRC -> localToGlobalCoords lRC gRC) t
    loop acc list =
      case list of
        pos :: xs -> loop (Dict.insert pos (Just (pos, 1)) acc) xs
        [] -> acc
  in
    let
      newBoard = loop board globals
    in newBoard 
  

setActivePiece : Model -> Position -> Tetrimino -> Model
setActivePiece ({activeBlock} as model) ((r,c) as p) t =
  if collidesWithWalls (p, t) then
    model 
  else
    { model | activeBlock = Just (p, t) }
    
  
getPiece : Int -> Tetrimino
getPiece x = Maybe.withDefault i (getAt x tetriminos)


gravity : Position -> Position
gravity (r, c) = (r+1, c)


emptyModel : Model
emptyModel = 
  Model emptyBoard emptyPieces emptyPieces Nothing Nothing Nothing 0 500 False


emptyBoard : Board
emptyBoard =
  Dict.fromList <| [1..h] `andThen` \x -> [1..w] `andThen` \y -> [(x => y => Nothing)]


emptyPieces : Board
emptyPieces = fromList []


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