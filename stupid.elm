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
        Just (((r, c), t) as block) ->
          let
            coords = blockToBricks ((pr,pc),t)
            
            bricksToInts = List.map (\((pr, _), _) -> pr) coords
            
            rows = List.foldl (\cur acc ->
              Dict.update cur maybeAddOne acc
            ) model.rows bricksToInts
            
            
          in
            if collidesWithPieces pieces block || collidesWithGround block
            then
              { model
              | rows = rows
              , pieces = (setPiece pieces ((pr, pc), t))
              , activeBlock = Nothing
              } => Task.perform never (\_ -> RandomPiece) (succeed always)

            else next => Cmd.none --Task.perform never (\_ -> CheckTetris) (succeed always)

        Nothing -> model => Cmd.none
      
    CheckTetris ->
      let
        fullRows = 
          List.map fst 
            <| List.filter (\(_,s) -> s == 10) -- filter out all rows without 10 blocks
            <| List.reverse 
            <| Dict.toList
            <| model.rows
        
      in      
        if (List.length fullRows) == 0 then
          model => Cmd.none
          
        else
          let
            removeKeysFromDict : Dict.Dict comparable -> List comparable -> Dict.Dict a
            removeKeysFromDict dict keys =
              case keys of
                k :: ks -> removeKeysFromDict (Dict.remove k dict) ks
                [] -> dict
                
            pcs = loop Dict.empty fullRows
          
            loop acc list =
              case list of
                fr :: xs ->
                  let
                    reducer : Position -> Maybe Brick -> Board -> Board
                    reducer (r,c) mB acc =
                      if fr == r then
                        acc
                        
                      -- I need a flag to mark certain rows for removal from model.rows,
                      -- if they've been removed from the board. i think that's the problem. 
                        
                      else if r < fr then
                        let pos = (r+1, c)
                        in Dict.insert pos (Just (pos, 11)) acc
                        
                      else
                        Dict.insert (r, c) (Just ((r, c), 1)) acc
                  in
                    loop (Dict.foldl reducer acc model.pieces) xs  
                  
                [] -> acc
                
          in { model
             | rows = removeDictKeys model.rows fullRows 
             , pieces = pcs
             } => Cmd.none

        
                
        -- o(n)^2 i think.
        -- iterate over rows :
          -- collect sizes of each row
            -- fold over rows : starting at bottom (22), removing full rows, using removedRowCountSoFar as a kind of accumulator
              -- { row | position = (r-removedRowCountSoFar, c)}
          
        -- will probably want to keep track of exiting rows 
  
    
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