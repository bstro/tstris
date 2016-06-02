module View exposing (view)

import Svg exposing (..)
import Svg.Lazy exposing (..)
import Svg.Attributes as Attr exposing (..)
import Dict exposing (values)
import List

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
            w  = toString <| width
            h  = toString <| height  
          in
            svg 
              [ viewBox "0 0 12 24" -- for some reason this centers it. where are the two extra columns coming from?
              , Attr.width (w ++ "px")
              , Attr.height (h ++ "px")
              , Attr.style "overflow: hidden; position: absolute;" 
              ]              
              [ lazy layout (Dict.values <| Dict.map maybeBrickToBrick emptyBoard) -- empties
              , lazy layout (Dict.values <| Dict.map maybeBrickToBrick pieces) -- placed pieces
              , lazy layout <| ghostifyBlock pieces block
              , lazy layout <| blockToBricks block
              ]
        Nothing -> Svg.text "no activeblock"
    Nothing -> Svg.text "no resolution" 


layout : List Brick -> Svg a
layout bricks = Svg.g [] <| List.map renderBrick bricks
 

renderBrick : Brick -> Svg a
renderBrick ((yy,xx), v) =
  let
    xxOffset = toFloat xx + toFloat gS / 2 |> toString
    yyOffset = toFloat yy + toFloat gS / 2 |> toString
    hx =
      case v of
        1     -> "#2C313A"
        10    -> "#E8C0ED"
        4     -> "#0a70f5"
        _     -> "#E6E6E6"
  in
  
  Svg.g 
  []
  
  [ rect
  [ fill hx
  , x (toString <| xx)
  , y (toString <| yy)
  , Attr.width "0.95"
  , Attr.height "0.95"
  ] []
  
  -- , Svg.text' [ textAnchor "middle", fontWeight "bold", fontFamily "Helvetica", fill "black", fontSize "0.25", x xxOffset, y yyOffset ] [Svg.text ((toString <| yy) ++ " : " ++ (toString <| xx))]   
  ]
