module View exposing (view)

import Html exposing (div, text, button, Html)
import List exposing (foldl, foldr)
import Element exposing (..)
import Collage exposing (..)
import Dict exposing (values)
import Color
import Transform

import Model exposing (..)
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
        piece = Maybe.withDefault 
      in
        Just <| collage width height
             <| [ groupTransform xf (renderBoard board)
                , group <| renderBlock board.activeBlock
                -- , group <| renderBlock <| Just ((-4,-11), l)
                ]
    
    Nothing -> Nothing
    


renderBoard {positions, activeBlock} =
  values <| Dict.map (\(r,c) v ->
    let
      y = (toFloat c * gS)
      x = (toFloat r * gS)
    in
    move (x,y) <| group <| (renderBlock v)
  ) positions
  
  
square : Shape
square = rect gS gS


shape : Color.Color -> Form
shape color = square |> (filled <| color)


renderBlock : Maybe Block -> List Form
renderBlock block =
  case block of
    Just ((oX, oY), t) ->
      let
        f = 
          if List.length t > 1 then
            List.map (\(x, y) ->
              let
                xx = (toFloat <| x) * gS
                yy = (toFloat <| y) * gS
              in
                move (xx, yy) (shape Color.orange)) t
          else [(shape Color.red)]
        nX = (toFloat (-3+oX))*gS
        nY = (toFloat (11-oY))*gS
        xf = Transform.translation nX nY

      in [groupTransform xf f]
                  
    Nothing -> [shape Color.gray]
    
-- xf = Transform.translation (oX+gS/2) (oY+gS/2)
-- xformed = groupTransform xf forms