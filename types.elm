module Types exposing (..)
import Window
import Mouse
import Time
import Dict exposing (Dict)

type alias Position = (Int, Int)

type alias Tetrimino = List Position
  
type alias Board =
  { positions: Dict Position (Maybe Tetrimino)
  , activeBlock : Maybe (Position, Tetrimino) 
  }
  
type alias Model =
  { board : Board
  , resolution : Maybe Window.Size
  , mouse : Maybe Mouse.Position
  , timeout : Float 
  } 

type Msg
  = NoOp
  | Init Window.Size
  | Resize Window.Size
  | MouseMove Mouse.Position
  | Rotate Tetrimino
  | Tick Time.Time