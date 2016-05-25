module View exposing (view)

import Html exposing (div, text, button, Html)
import Element exposing (..)
import Collage exposing (..)
import Text exposing (..)
import Color
import Transform

import Model exposing (..)
import Types exposing (..)
import Utilities exposing (..)


square = rect gS gS


view model =
  let forms = layout model
  in case forms of
    Just forms ->
      toHtml forms
      
    Nothing -> Html.text "Nothing" 
 
  
layout model =
  case model.resolution of
    Just {width, height} ->
      let
        oX = -(toFloat width)/2
        oY = -(toFloat height)/2
        forms = List.map showBlock model.board
        -- xf = Transform.translation (oX+gS/2) (oY+gS/2)
        -- xformed = groupTransform xf forms
      in
      Just <| collage width height forms -- [xformed]
    
    Nothing -> Nothing
      -- collage 0 0 [showBlock Nothing]


stringifyCoords coords = toForm <| show (Text.fromString (toString coords)) 


showBlock : Maybe Block -> Form
showBlock block =
  let shape = square |> (filled <| Color.red)
  in
  case block of
    Just coords ->
      group <| List.map (\(x, y) ->
        let
          xx = (toFloat x) * gS
          yy = (toFloat y) * gS
        in move (xx, yy) shape) (rotatePiece coords)          

    Nothing -> shape


-- showBlock : Maybe (List Block) -> List Form
-- showBlock block =
--   case block of
--     Just list ->
--       List.map (\c ->
--         square |> (filled <| Color.red) |> move (100,100)
--       ) list
      
--     Nothing ->
--       [square |> (filled <| Color.charcoal)]
      