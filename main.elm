import Html.App as Html
import Window
import Mouse
import Keyboard
import Task exposing (succeed)
import Basics.Extra exposing (never)
import List.Extra exposing (getAt)
import AnimationFrame
import Random

import View exposing (view)
import Types exposing (..)
import Model exposing (..)
import Utilities exposing (..)

main =
  Html.program
    { view = view
    , update = update
    , init = init
    , subscriptions = subscriptions
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes Resize 
    , Mouse.moves MouseMove
    , AnimationFrame.diffs Tick
    , Keyboard.downs KeyDown
    ]

    
init =
  setActivePiece emptyModel (0,0) t
  => Task.perform never Init Window.size


setActivePiece : Model -> Position -> Tetrimino -> Model
setActivePiece ({board, activeBlock} as model) p t =
  { model | activeBlock = Just (p, t) }
  

getPiece : Int -> Tetrimino
getPiece x = Maybe.withDefault i (getAt x tetriminos)


gravity : Position -> Position
gravity (x, y) = (x, y-1)


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({board, activeBlock, level} as model) =
  case msg of
    NoOp
    -> model
    => Cmd.none
    
    Init res
    -> ({ model | resolution = Just res })
    => Cmd.none
    
    Tick time ->
    let
      t = round time
    in
      if model.timeout < model.level
      then { model | timeout = model.timeout+t } => Cmd.none
      else { model | timeout = 0 } =>
        case model.activeBlock of
          Just b -> Task.perform never Step (succeed b)
          Nothing -> Cmd.none

    Step (p, t)
    ->
      case model.skipNextTick of
        True -> { model | skipNextTick = False } => Cmd.none
        False ->
          let 
            c = gravity p
            ({activeBlock} as next) = setActivePiece model c t
            new = setActivePiece model
          in 
          case next.activeBlock of
            Just ((x,y), t) ->
              let debug = Debug.log "y < h : " (y,h) in
              if abs y < h then next => Cmd.none 
              else { model | activeBlock = Nothing } => Cmd.none

            Nothing -> model => Cmd.none


    KeyDown code ->
      case model.activeBlock of
        Just ((x,y), t) ->
          let next =
            case code of
              37 -> setActivePiece model (x-1, y) t -- l
              38 -> setActivePiece model (x  , y) (List.map rotateR t) -- u
              39 -> setActivePiece model (x+1, y) t -- r
              40 -> setActivePiece model (x  , y) (List.map rotateL t) -- d
              32 ->
                let tmp = setActivePiece model (x  , y-1) t -- sp
                in  { tmp | skipNextTick = True }
              _  -> model
              
          in next => Cmd.none
          
        Nothing -> model => Cmd.none
        
    RotateR (p, t)
    -> setActivePiece model p (List.map rotateR t)
    => Cmd.none
    
    RotateL (p, t)
    -> setActivePiece model p (List.map rotateL t)
    => Cmd.none
    
    NextLevel
    -> ({ model | level = level-100 })
    => Cmd.none
    
    MouseMove pos
    -> ({ model | mouse = Just pos})
    => Cmd.none
    
    Resize newRes
    -> ({ model | resolution = Just newRes })
    => Cmd.none

    RandomPiece
    -> model
    => (Random.generate NewPiece (Random.int 0 6))

    NewPiece r
    -> (setActivePiece model (0,0) (getPiece r))
    => Cmd.none