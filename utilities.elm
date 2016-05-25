module Utilities exposing (..)

import Model exposing (..)
import Types exposing (..)


(=>) : a -> b -> (a , b)
(=>) = (,)


getRow : Int -> Int
getRow idx = idx // w
    
    
getCol : Int -> Int
getCol idx = idx // h


-- rotateMaybe piece =
--   case piece of
--     Just piece -> rotatePiece piece
--     Nothing -> [(0,0)]


rotatePiece : Block -> Block
rotatePiece piece = 
  List.map rotate piece

  
rotate (x, y) = (-y, x)