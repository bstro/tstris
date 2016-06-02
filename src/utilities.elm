module Utilities exposing (..)

import Color exposing (..)
import Dict exposing (Dict)

import Types exposing (..)


gravity : Position -> Position
gravity (r, c) = (r+1, c)


maybeAddOne mV =
  case mV of
    Just v -> Just (v+1)
    _ -> Just 1    


collisionsAtRow : Board -> Block -> Int -> Bool
collisionsAtRow pieces (((r,c), t) as block) row =
  let 
    bricks = blockToBricks ((row,c), t)
  in
    collidesWithPieces pieces bricks || collidesWithGround bricks
    


firstAvailableRow : Board -> Block -> Int
firstAvailableRow pieces block =
  let
    loop row =
      let collisions = collisionsAtRow pieces block row in
      case collisions of
        True -> loop (row-1)
        _ -> row
  
  in loop h


ghostifyBlock : Board -> Block -> List Brick
ghostifyBlock pieces (((_,c), t) as block) =
  let 
    r = firstAvailableRow pieces block  
  in
    blockToBricks ((r,c),t)
    
 
maybeBrickToBrick : Position -> Maybe Brick -> Brick
maybeBrickToBrick pos mB =
  case mB of
    Just brick -> brick
    Nothing -> (pos, -1)
 

blockToBricks : Block -> List Brick
blockToBricks (gRC, t) =
  let
    loop acc list =
      case list of
        lRC :: xs ->
          loop (acc ++ [(localToGlobalCoords lRC gRC, List.length t)]) xs
        _ -> 
          acc
  in
  loop [] t


blocksToBricks : List Block -> List Brick
blocksToBricks blocks = List.concatMap blockToBricks blocks


collidesWithSides : List Brick -> Bool
collidesWithSides bricks = List.any (\((r, c), _) ->  c == 0 || c > w) bricks 


collidesWithGround : List Brick -> Bool
collidesWithGround bricks = List.any (\((r, c), _) ->  r > h) bricks


collidesWithPieces : Board -> List Brick -> Bool
collidesWithPieces pieces bricks = List.any (\(g, _) -> Dict.member g pieces) bricks


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

