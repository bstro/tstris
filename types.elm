module Types exposing (..)
import Window
import Mouse
import Time

type alias Model =
  { board : List (Maybe Block)
  , resolution : Maybe Window.Size
  , mouse : Maybe Mouse.Position
  , activeBlock : Maybe Block
  , timeout : Float 
  } 


type alias Block = List (Int, Int)


type Msg
  = NoOp
  | Init Window.Size
  | Resize Window.Size
  | MouseMove Mouse.Position
  -- | MouseClick Mouse.Position
  | RotateR Mouse.Position
  | RotateL Block
  | Tick Time.Time