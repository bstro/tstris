module Update exposing (update)

import Task exposing (succeed)
import Basics.Extra exposing (never)
import Random
import Dict

import Utilities exposing (..)
import Model exposing (..)
import Types exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({board, activeBlock, pieces} as model) =
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
      if model.timeout < 500
      then { model | timeout = model.timeout+t } => Cmd.none
      else { model | timeout = 0 } =>
        case model.activeBlock of
          Just b ->
            let state = collisionsAtRow pieces b 0 in  
            Task.perform never Step (succeed (b, state))
          
          Nothing -> Cmd.none

    Step ((p, t), state) ->
      case state of
        True ->
          { model | dead = True } => Cmd.none
        False ->
          case model.skipNextTick of
            True -> { model | skipNextTick = False } => Cmd.none
            False ->
              let next = setActivePiece model (gravity p) t
              in
              model => Task.perform never (\_ -> CheckStep next p) (succeed always)

    CheckStep next (pr,pc) ->
      case next.activeBlock of
        Just ((((cr, cc), t) as nextBlock)) ->
          let
            -- following three lines are for building up `rows` incrementally
            coords = blockToBricks ((pr, pc), t)
            bricksToInts = List.map (\((pr, _), _) -> pr) coords
            rows = List.foldl (\cur acc -> Dict.update cur maybeAddOne acc) model.rows bricksToInts
            
            nextPositionCoords = blockToBricks ((cr, cc), t)
          in
          
          if collidesWithPieces pieces nextPositionCoords || collidesWithGround nextPositionCoords then
            if cc-1 == pc || cc+1 == pc then -- if horizontal move collision
              model => Cmd.none
            else
              { model
              | rows = rows
              , pieces = (setPiece pieces ((pr, pc), t))
              , activeBlock = Nothing
              } => Task.perform never (\_ -> RandomPiece) (succeed always)

          else next => Cmd.none

        Nothing -> model => Cmd.none

    ClearRows ->
      let
        fullRows =
          model.rows
          |> Dict.toList
          |> List.reverse
          |> List.filter (\(_, s) -> s == 10) -- filter out all rows without 10 blocks
          |> List.map fst

        foldOverModel (r,c) v acc =
          let
            rowOffset = List.length <| List.filter (\fr -> r < fr) fullRows
          in
            if List.member r fullRows then
              { acc | outgoing = Dict.insert (r, c) (Just ((r,c), 1)) acc.outgoing }
            else
              let nr = r+rowOffset in -- nr -> new row
                { acc
                | pieces = Dict.insert (nr, c) (Just ((nr, c), 1)) acc.pieces
                , rows = Dict.update nr maybeAddOne acc.rows
                }
      in
        if List.length fullRows > 0 then
          Dict.foldr
            foldOverModel
              { model
              | rows = Dict.empty
              , outgoing = Dict.empty
              , pieces = Dict.empty
              , cleared = model.cleared + (List.length fullRows)
              } model.pieces
          => Cmd.none

        else
          model => Cmd.none


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
                let tmp = setActivePiece model (r+1, c) t -- sp
                in  { tmp | skipNextTick = True }

              32 ->
                let fOR = firstUnobstructedRow model.pieces ((r,c),t) in 
                setActivePiece model (fOR, c) t

              _  ->
                model

          in model => Task.perform never (\_ -> CheckStep next (r,c)) (succeed always)

        Nothing -> model => Cmd.none

    Rotate (p, t)
    -> setActivePiece model p (List.map rotateR t)
    => Cmd.none

    Resize newRes
    -> ({ model | resolution = Just newRes })
    => Cmd.none

    RandomPiece
    -> model
    => (Random.generate InsertPiece (Random.int 0 6))

    InsertPiece r
    -> (setActivePiece model (0, 6) (getPiece r))
    => Task.perform never (\_ -> ClearRows) (succeed always)