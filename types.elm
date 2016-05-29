module Types exposing (..)
import Window
import Keyboard exposing (KeyCode)
import Mouse
import Time
import Dict exposing (Dict)

type alias Position = (Int, Int)

type alias Tetrimino = List (Int, Int)
  
type alias Block = (Position, Tetrimino)

type alias Brick = (Position, Int)
 
type alias Board = Dict Position (Maybe Brick)
  
type alias Model =
  { board : Board
  , pieces : Board
  , ghostPieces : Board
  , activeBlock : Maybe Block
  , mouse : Maybe Mouse.Position  
  , resolution : Maybe Window.Size
  , timeout : Int 
  , level : Int
  , skipNextTick : Bool
  } 

type Msg
  = NoOp
  | Init Window.Size
  | Resize Window.Size
  -- | MouseMove Mouse.Position
  | CheckStep Model
  | KeyDown KeyCode
  | Step Block
  | RotateL Block
  | RotateR Block
  | Tick Time.Time
  | RandomPiece
  | NewPiece Int
  | NextLevel
  