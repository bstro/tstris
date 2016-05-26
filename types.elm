module Types exposing (..)
import Window
import Mouse
import Time
import Dict exposing (Dict)

type alias Position = (Int, Int)

type alias Tetrimino = List (Int, Int)
  
type alias Block = (Position, Tetrimino)

-- type alias Brick = Int
 
type alias Board =
  { positions: Dict Position (Maybe Block)
  , activeBlock : Maybe Block
  }
  
type alias Model =
  { board : Board
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
  | Step Block
  | Rotate Block
  | Tick Time.Time
  | GetNumber
  | NewPiece Int
  | NextLevel
  