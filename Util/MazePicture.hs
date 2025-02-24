module Util.MazePicture(mazeToPicture) where

import Util.Maze
import Graphics.Gloss
import Util.Location
import qualified Data.Vector as V

mazeToPicture :: Int -> Maze a -> (a -> Picture) -> (Int, Int, Picture)
mazeToPicture cellSize maze drawCell =
    (windowWidth, windowHeight, pic)
    where  (Maze cells) = maze
           windowHeight = cellSize * V.length cells
           windowWidth = cellSize * V.length (V.head cells)
           locatedCell (Located (Location x y) c) = translate (fromIntegral $ x * cellSize) (fromIntegral $ y * cellSize) $ drawCell c
           pic = translate 
            (fromIntegral $ cellSize `div` 2) 
            (fromIntegral $ cellSize `div` 2) 
            $ Pictures $ map locatedCell $ items maze