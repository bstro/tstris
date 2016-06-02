module Types exposing (..)
import Window
import Keyboard exposing (KeyCode)
import Mouse
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
  , mouse : Maybe Mouse.Position  
  , resolution : Maybe Window.Size
  , timeout : Int 
  , skipNextTick : Bool
  , rows : Dict Int Int
  , cleared : Int
  } 

type Msg
  = NoOp
  | Init Window.Size
  | Resize Window.Size
  | CheckStep Model Position-- model is `next` model in this case
  | ClearRows
  | KeyDown KeyCode 
  | Step Block
  | RotateL Block
  | RotateR Block
  | Tick Time.Time
  | RandomPiece
  | InsertPiece Int
  