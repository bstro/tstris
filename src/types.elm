module Types exposing (..)
import Window
import Keyboard exposing (KeyCode)
import Time
import Dict exposing (Dict)

type alias Position = (Int, Int)

type alias Positions = Dict Position (Maybe Tetrimino)

type alias Tetrimino = List (Int, Int)
  
type alias Block = (Position, Tetrimino)

type alias Brick = (Position, Int)
 
type alias Board = Dict Position (Maybe Brick)

type alias Model =
  { board : Board
  , pieces : Board
  , ghostPieces : Board
  , outgoing : Board
  , activeBlock : Maybe Block  
  , resolution : Maybe Window.Size
  , timeout : Int 
  , skipNextTick : Bool
  , rows : Dict Int Int
  , cleared : Int
  , dead : Bool
  } 

type Msg
  = NoOp
  | Init Window.Size
  | Resize Window.Size
  | CheckStep Model Position
  | ClearRows
  | KeyDown KeyCode 
  | Step (Block, Bool)
  | Rotate Block
  | Tick Time.Time
  | InsertPiece Int
  | RandomPiece
  