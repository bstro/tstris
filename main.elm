import Html.App as Html
import Window
import Mouse
import Keyboard
import Task exposing (succeed)
import Basics.Extra exposing (never)
import List.Extra exposing (getAt)
import AnimationFrame
import Random
import Dict

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
    -- , Mouse.moves MouseMove
    , AnimationFrame.diffs Tick
    , Keyboard.downs KeyDown
    ]


init =
  setActivePiece emptyModel (0,6) i
  => Task.perform never Init Window.size


setPiece : Board -> Block -> Board
setPiece board (gRC, t) =
  let 
    globals = List.map (\lRC -> localToGlobalCoords lRC gRC) t
    loop acc list =
      case list of
        pos :: xs -> loop (Dict.insert pos (Just (pos, 1)) acc) xs
        [] -> acc
  in
    let
      newBoard = loop board globals
    in newBoard 
  

setActivePiece : Model -> Position -> Tetrimino -> Model
setActivePiece ({activeBlock} as model) ((r,c) as p) t =
  if collidesWithWalls (p, t) then
    model 
  else
    { model | activeBlock = Just (p, t) }
    
  
getPiece : Int -> Tetrimino
getPiece x = Maybe.withDefault i (getAt x tetriminos)


gravity : Position -> Position
gravity (r, c) = (r+1, c)


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({board, activeBlock, level, pieces} as model) =
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

    Step (p, t) ->
      case model.skipNextTick of
        True -> { model | skipNextTick = False } => Cmd.none
        False ->
          let 
            c = gravity p
            next = setActivePiece model c t
          in
          model => Task.perform never (\_ -> CheckStep next) (succeed always)

    CheckStep next ->
      case next.activeBlock of
        Just (((r, c), t) as block) ->
          if collidesWithPieces pieces block || collidesWithGround block
          then
            { model 
            | pieces = (setPiece pieces ((r-1, c), t)) -- subtracting one from the row seems like a massive hack. this will probably bite me in the ass later
            , activeBlock = Nothing
            } => Task.perform never (\_ -> RandomPiece) (succeed always)
                            
          else next => Cmd.none

        Nothing -> model => Cmd.none
      

    KeyDown code ->
      case model.activeBlock of
        Just ((r,c), t) ->
          let next =
            case code of
              37 ->
                setActivePiece model (r, c-1) t
                
              38 -> 
                setActivePiece model (r, c) (List.map rotateR t)
                
              39 -> 
                setActivePiece model (r, c+1) t
                
              40 -> 
                setActivePiece model (r, c) (List.map rotateL t)
                
              32 ->
                let tmp = setActivePiece model (r+1, c) t -- sp
                in  { tmp | skipNextTick = True }
                
              _  -> 
                model
              
          in model => Task.perform never (\_ -> CheckStep next) (succeed always)
          
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
    
    -- MouseMove pos
    -- -> ({ model | mouse = Just pos})
    -- => Cmd.none
    
    Resize newRes
    -> ({ model | resolution = Just newRes })
    => Cmd.none

    RandomPiece
    -> model
    => (Random.generate NewPiece (Random.int 0 6))

    NewPiece r
    -> (setActivePiece model (0, 6) (getPiece r))
    => Cmd.none