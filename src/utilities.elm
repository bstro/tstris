module Utilities exposing (..)

import Color exposing (..)
import Dict exposing (Dict)

import Types exposing (..)


maybeAddOne mV =
  case mV of
    Just v -> Just (v+1)
    _ -> Just 1    


-- need a function called lowestBrickOnBoard 
-- ... or getFirstAvailableBrick and map (r,_) to that rather than 0 (dumb)
ghostifyBrick : Brick -> Brick
ghostifyBrick ((r,c), b) = ((h+1, c), w)
  
 
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

