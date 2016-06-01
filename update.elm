module Update exposing (update)

import Task exposing (succeed)
import Basics.Extra exposing (never)
import Random
import Dict

import Utilities exposing (..)
import Model exposing (..)
import Types exposing (..)

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
          let next = setActivePiece model (gravity p) t
          in
          model => Task.perform never (\_ -> CheckStep next p) (succeed always)

    CheckStep next (pr,pc) ->      
      case next.activeBlock of
        Just ((_, mb) as nextBlock) ->
          let
            coords = blockToBricks ((pr,pc), mb)
            bricksToInts = List.map (\((pr, _), _) -> pr) coords
            rows = List.foldl (\cur acc -> Dict.update cur maybeAddOne acc) model.rows bricksToInts
          in
          
          if collidesWithPieces pieces nextBlock || collidesWithGround nextBlock then
            { model
            | rows = rows
            , pieces = (setPiece pieces ((pr, pc), mb))
            , activeBlock = Nothing
            } => Task.perform never (\_ -> RandomPiece) (succeed always)

          else next => Cmd.none

        Nothing -> model => Cmd.none
      
    CheckTetris ->
      let
        fullRows = 
          List.map fst 
            <| List.filter (\(_, s) -> s == 10) -- filter out all rows without 10 blocks
            <| List.reverse 
            <| Dict.toList
            <| model.rows
            
        
        log = Debug.log "fullRows are" fullRows
            
        overPieces fullRow acc =
          let log = Debug.log "fullRow is" fullRow in
          Dict.foldr (\(r,c) v a ->
            if r == fullRow then a
            else if r < fullRow then 
              { a
              | pieces = Dict.insert (r+1, c) (Just ((r+1,c), 1)) a.pieces -- move columns forward a row in BOTH `rows` and `pieces`
              , rows = Dict.update (r+1) maybeAddOne a.rows  
              }
            else
              { a
              | pieces = Dict.insert (r,c) v a.pieces
              , rows = Dict.update r maybeAddOne a.rows
              }
          ) acc model.pieces
                   
      in
      if List.length fullRows > 0 then
        (List.foldl overPieces { model | pieces = Dict.empty, rows = Dict.empty } fullRows)
        => Cmd.none
      else
        model => Cmd.none
    
      -- model => Cmd.none
      
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
              
          in model => Task.perform never (\_ -> CheckStep next (r,c)) (succeed always)
          
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
     
    Resize newRes
    -> ({ model | resolution = Just newRes })
    => Cmd.none

    RandomPiece
    -> model
    => (Random.generate InsertPiece (Random.int 0 6))

    InsertPiece r
    -> (setActivePiece model (0, 6) (getPiece r))
    => Task.perform never (\_ -> CheckTetris) (succeed always)