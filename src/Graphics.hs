module Graphics where

import Game
import Data
import Data.Array
import Graphics.Gloss

bluePlain :: Picture
bluePlain = 
  translate (gridWidth / 2) (gridHeight / 2) $ 
  color blue $ 
  rectangleSolid gridWidth gridHeight

drawWinnigSlot :: ((Int,Int),Slot) -> Picture
drawWinnigSlot ((x,y),s) = 
  translate 
    (fromIntegral x * slotWidth + slotWidth /2) 
    (fromIntegral y * slotHeight + slotHeight / 2) $ 
  color white $ 
  thickCircle 10 slotRadius


drawSlot :: ((Int,Int),Slot) -> Picture
drawSlot ((x,y),s) = 
  translate 
    (fromIntegral x * slotWidth + slotWidth /2) 
    (fromIntegral y * slotHeight + slotHeight / 2) $ 
  color slotColor $ 
  circleSolid slotRadius
  where 
    slotColor =
      case s of
        Slot Nothing -> black
        Slot (Just Red) -> red
        Slot (Just Yellow) -> yellow

drawSlots :: Grid -> Picture
drawSlots (Grid grid) = 
  pictures $
  map drawSlot $
  assocs grid

drawSelectedColumn :: Maybe Player -> Maybe Column -> Picture
drawSelectedColumn Nothing _ = Blank
drawSelectedColumn _ Nothing = Blank
drawSelectedColumn player (Just column) =
  drawSlot ((column,6),Slot player)

drawDropCoinPhase :: Game -> Picture
drawDropCoinPhase game =
  translate 
    (-0.5 * fromIntegral screenWidth) 
    (-0.5 * fromIntegral screenHeight) $ 
  pictures 
  [ bluePlain
  , drawSlots $ gameGrid game
  , drawSelectedColumn (currentPlayer game) (columnUnderMouse game)
  ]

drawWinnerLines :: Grid -> [(Int,Int)] -> Picture
drawWinnerLines (Grid grid) winnerLines =
  pictures $
  map drawWinnigSlot $
  filter (\(pos,_) -> pos `elem` winnerLines) $
  assocs grid

drawGameoverPhase :: Game -> [(Int,Int)] -> Picture
drawGameoverPhase game winnerLines =
 translate 
    (-0.5 * fromIntegral screenWidth) 
    (-0.5 * fromIntegral screenHeight) $ 
 pictures 
  [ bluePlain
  , drawSlots $ gameGrid game
  , drawWinnerLines grid winnerLines]
  where grid = gameGrid game

drawGame :: Game -> Picture
drawGame game = 
  case gameState game of
    DropCoin -> drawDropCoinPhase game
    Gameover xs -> drawGameoverPhase game xs
  

