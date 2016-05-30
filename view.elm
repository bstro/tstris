module View exposing (view)

import Html
import Window exposing (Size)
import Svg exposing (..)
import Svg.Lazy as Lazy
import Svg.Attributes as Attr exposing (..)
import Dict exposing (values)
import List exposing (length)

import Model exposing (..)
import Types exposing (..)
import Utilities exposing (..)


view : Model -> Svg Msg
view ({board, activeBlock, resolution, pieces, ghostPieces} as model) =
  case resolution of
    Just ({width, height} as res) ->
      case activeBlock of
        Just (((r,c), t) as block) ->
          let
            w  = toString width
            h  = toString height
            rz = "0" ++ " " ++ "0" ++ " " ++ w ++ " " ++ h  
          in
            svg 
              [ viewBox "0 0 12 24" -- for some reason this centers it. where are the two extra columns coming from?
              , Attr.width (w ++ "px")
              , Attr.height (h ++ "px")
              ]
              -- <| List.concat
              [ layout <| Dict.values <| Dict.map maybeBrickToBrick emptyBoard
              , layout <| Dict.values <| Dict.map maybeBrickToBrick pieces
              , layout <| blockToBricks <| block 
              , layout <| List.map ghostifyBrick <| blockToBricks <| block
              ]
        Nothing -> Svg.text "no activeBlock"
    Nothing -> Svg.text "no resolution" 


layout : List Brick -> Svg a
layout bricks =
  Svg.g [] <| List.map renderBrick bricks
 

renderBrick : Brick -> Svg a
renderBrick ((yy,xx), v) =
  let
    sq = toString gS
    hx =
      case v of
        10   -> "#F9F9F9"
        1    -> "red"
        4    -> "blue"
        _    -> "#E6E6E6"
  in
  rect [ fill hx
       , x (toString <| xx)
       , y (toString <| yy)
       , Attr.width "0.95"
       , Attr.height "0.95"
       ] []


-- blockHeight : Block -> Int    
-- blockHeight (_, t) =
--   let loop acc list =
--     case list of
--       (r,c) :: xs ->
        
       
--       _ -> acc
      
  -- in loop 0 t
  --  ) t

-- blockWidth : Block -> Int

    

ghostifyBrick : Brick -> Brick
ghostifyBrick ((r,c), b) =
  ((23,c), 10)
  
  -- need a function called lowestBrickOnBoard ... or getFirstAvailableBrick and map (r,_) to that rather than 0 (dumb)
   

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
          loop (acc ++ [(localToGlobalCoords lRC gRC, length t)]) xs --acc ++ [(localToGlobalCoords lRC gRC, length t)] ++ loop
        _ -> 
          acc
  in loop [] t


blocksToBricks : List Block -> List Brick
blocksToBricks blocks =
  List.concatMap blockToBricks blocks
 
 
   