module View exposing (view)

import Html exposing (div, text, button, Html)
-- import List exposing (foldl, foldr)
import Element exposing (Element, toHtml)
import Collage exposing (..)
import Dict exposing (values)
import Color exposing (..)
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
layout ({board, activeBlock, resolution} as model) =
  case resolution of
    Just {width, height} ->
      let
        forms = group <| renderBoard board
        oX = toFloat -(gS*w)/2
        oY = toFloat -(gS*h)/2
        xf = Transform.translation oX oY
        activeForm = renderActiveBlock activeBlock
        layers = [forms, activeForm]
      in
      Just <| collage width height [Collage.move (oX, oY) <| group <| layers] 
      
    _ -> Nothing


localToGlobalXY (lX, lY) (gX, gY) = (lX+gX , lY+gY)


renderBlock : Position -> Block -> Color -> Form
renderBlock ((gX, gY) as gXY) (_, points) color =
  let
    globals = List.map (\lXY -> localToGlobalXY lXY gXY) points
    forms = List.map (\(r,c) ->
      let
        x = toFloat c*gS
        y = toFloat r*gS
      -- in move (x,y) <| toForm <| Element.leftAligned <| Text.style type1 <| Text.fromString (toString r ++ ":" ++ toString c)
      in move (x,y) <| shape color
    ) globals
  in 
  group forms
  -- move (xx,yy) <| shape color


renderActiveBlock : Maybe Block -> Form
renderActiveBlock block =
  case block of
    Just (((x,y), t) as v) -> renderBlock (x,y) v red 
    Nothing -> Collage.toForm Element.empty


renderBoard : Board -> List Form
renderBoard board =
  let
    cellToForm gXY mT =
      case mT of
        Just block -> renderBlock gXY block red
        Nothing -> renderBlock gXY (gXY, [(0,0)]) gray
  in
  values <| Dict.map cellToForm board