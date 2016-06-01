module View exposing (view)

import Svg exposing (..)
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
              [ layout <| Dict.values <| Dict.map maybeBrickToBrick emptyBoard
              , layout <| Dict.values <| Dict.map maybeBrickToBrick pieces
              , layout <| blockToBricks <| block 
              -- , layout <| List.map ghostifyBrick <| blockToBricks <| block
              ]
        Nothing -> Svg.text "no activeblock"
    Nothing -> Svg.text "no resolution" 


layout : List Brick -> Svg a
layout bricks = Svg.g [] <| List.map renderBrick bricks
 

renderBrick : Brick -> Svg a
renderBrick ((yy,xx), v) =
  let
    sq = toString gS
    hx =
      case v of
        11   -> "purple"
        10   -> "#F9F9F9"
        1    -> "red"
        4    -> "blue"
        _    -> "#E6E6E6"
  in
  Svg.g 
  [ fill hx
  , x (toString <| xx)
  , y (toString <| yy)
  , Attr.width "0.95"
  , Attr.height "0.95"
  ]
  [ rect
    [ fill hx
    , x (toString <| xx)
    , y (toString <| yy)
    , Attr.width "0.95"
    , Attr.height "0.95"
    ] []
  , Svg.text' [ fontFamily "Helvetica", fill "black", fontSize "1", x "50", y "50" ] [Svg.text "x"]   
  ]
