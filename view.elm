module View exposing (view)

import Html exposing (div, text, button, Html)
import List exposing (foldl, foldr)
import Element exposing (..)
import Collage exposing (..)
import Dict exposing (values)
import Color
import Transform
import Text

import Model exposing (..)
import Types exposing (..)
import Utilities exposing (..)


view : Model -> Html Msg
view model =
  let element = layout model
  in case element of
    Just element ->
      toHtml element
      
    Nothing ->
      Html.text "Nothing" 
 
 
layout : Model -> Maybe Element
layout ({board, activeBlock, resolution, mouse} as model) =
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
                -- , groupTransform 
                , group <| renderBlock activeBlock
                ]
    
    Nothing -> Nothing
    

renderBoard : Board -> List Form
renderBoard board =
  values <| Dict.map (\(r,c) v ->
    let
      x = (toFloat r * gS)
      y = (toFloat c * gS)
      g = group <| renderBlock v
      t = Collage.toForm (show (x,y))
      i = group [g, t]
    in
    move (x,y) <| i
  ) board
  
  
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
                move (xx, yy) (Collage.toForm (show (x,y)))) t
                -- move (xx, yy) (shape Color.orange)) t
                
          else [(shape Color.red)]
        
        nX = (toFloat oX)*gS
        nY = (toFloat (oY + h//2))*gS
        xf = Transform.translation nX nY

      in [groupTransform xf f]
                  
    Nothing -> [shape Color.gray]
    
-- xf = Transform.translation (oX+gS/2) (oY+gS/2)
-- xformed = groupTransform xf forms