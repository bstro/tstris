module Types exposing (..)
import Window
import Keyboard exposing (KeyCode)
import Mouse
import Time
import Dict exposing (Dict)

type alias Position = (Int, Int)

type alias Tetrimino = List (Int, Int)
  
type alias Block = (Position, Tetrimino)
 
type alias Board = Dict Position (Maybe Block)
  
type alias Model =
  { board : Board
  , activeBlock : Maybe Block
  , mouse : Maybe Mouse.Position  
  , resolution : Maybe Window.Size
  , timeout : Int 
  , level : Int
  } 

type Msg
  = NoOp
  | Init Window.Size
  | Resize Window.Size
  | MouseMove Mouse.Position
  | KeyDown KeyCode
  | Step Block
  | Rotate Block
  | Tick Time.Time
  | RandomPiece
  | NewPiece Int
  | NextLevel
  