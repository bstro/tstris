module View exposing (view)

import Html exposing (div, text, button, Html)
import List exposing (foldl, foldr)
import Element exposing (..)
import Collage exposing (..)
import Dict exposing (values)
import Color
import Transform

-- import Model exposing (..)
import Types exposing (..)
import Utilities exposing (..)


view : Model -> Html Msg
view model =
  let forms = layout model
  in case forms of
    Just forms ->
      toHtml forms
      
    Nothing ->
      Html.text "Nothing" 
 
 
layout : Model -> Maybe Element
layout ({board, resolution, mouse} as model) =
  case model.resolution of
    Just {width, height} ->
      let
        oX = -(toFloat w)*gS/2
        oY = -(toFloat h)*gS/2
        xf = Transform.translation oX oY
      in
        Just <| collage width height
            <| [groupTransform xf (renderBoard board)]
    
    Nothing -> Nothing
    

renderBoard : Board -> List Form 
renderBoard {positions, activeBlock} =
  values <| Dict.map (\(r,c) v ->
    let
      y = toFloat c * gS
      x = toFloat r * gS
    in
    move (x,y) <| group <| (showBlock v)
  ) positions
  
  
square : Shape
square = rect gS gS


shape : Color.Color -> Form
shape color = square |> (filled <| color)


showBlock : Maybe Tetrimino -> List Form
showBlock block =
  case block of
    Just coords ->
      List.map (\(x, y) ->
        let
          xx = (toFloat x) * gS
          yy = (toFloat y) * gS
        in
          move (xx, yy) (shape Color.red)) coords          

    Nothing -> [shape Color.gray]
    
-- xf = Transform.translation (oX+gS/2) (oY+gS/2)
-- xformed = groupTransform xf forms