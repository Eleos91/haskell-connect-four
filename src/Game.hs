module Game where

import Data
import Data.Maybe
import Data.Array
import Graphics.Gloss.Interface.IO.Interact

initGrid :: Grid
initGrid =
  Grid $ 
  array ((0,0),(6,5)) 
  (zip (range ((0,0),(6,5))) 
       (repeat (Slot Nothing))) 

initGame :: Game
initGame = Game initGrid DropCoin (Just Red) Nothing

transformGame :: Event -> Game -> Game
transformGame (EventMotion x) game =
  updateMousePosition x game
transformGame (EventKey (MouseButton LeftButton) Down _ mousePos) game
  | validMouseCoord (translatedMouseX, translatedMouseY)
  && rowIsEmpty && gameState game == DropCoin =
    determineGameOver ((column,fromJust row), Slot $ currentPlayer game) $
      game { gameGrid = modifyGrid game (column, fromJust row) }
  | isGameover = initGame
  | otherwise = game
  where
    (translatedMouseX, translatedMouseY) = translateMouseCoord mousePos
    column = targetColumn $ fst mousePos
    (Grid grid) = gameGrid game
    rowIsEmpty = isJust row
    row = getRow grid column
    isGameover = 
      case gameState game of 
        Gameover _ -> True
        _ -> False
transformGame _ game = game

modifyGrid game (c,r) =
      Grid $ grid // [((c,r), Slot $ currentPlayer game)]
  where (Grid  grid) = gameGrid game

determineGameOver :: ((Int,Int),Slot) -> Game -> Game
determineGameOver newCoin game 
  | gotAWinner = game { gameState = Gameover testResult}
  | gridIsFull grid = game { gameState = Gameover [], currentPlayer = Nothing}
  | otherwise = game {currentPlayer = switchPlayer $ currentPlayer game}
  where
    grid = gameGrid game
    testResult = testFor4InALine grid newCoin
    gotAWinner = testResult /= []

gridIsFull :: Grid -> Bool
gridIsFull (Grid grid) =
  all (\((_,_),Slot slot) -> isJust slot) $
  filter (\((_,y),_) -> y == 5) $
  assocs grid

getRow :: Array (Int,Int) Slot -> Column -> Maybe Row
getRow grid column = getRow' grid (column,0)
getRow' :: Array (Int,Int) Slot -> (Column,Row) -> Maybe Row
getRow' arr (column,6) = Nothing
getRow' arr (column,x)
  | isEmptySlot $ arr ! (column,x) = Just x
  | otherwise = getRow' arr (column,x+1)
  where isEmptySlot (Slot x) = isNothing x

translateMouseCoord :: (Float,Float) -> (Float,Float)
translateMouseCoord (x,y) = (translatedX, translatedY)
  where
    translatedX = (fromIntegral screenWidth / 2 + x)
    translatedY = (fromIntegral screenHeight /2 + y)

validMouseCoord :: (Float, Float) -> Bool
validMouseCoord (x,y) =
  (x >= 0) && (x <= fromIntegral screenWidth) &&
  (y >= 0) && (y <= fromIntegral screenHeight)

switchPlayer :: Maybe Player -> Maybe Player
switchPlayer player =
  case player of
    Just Red -> Just Yellow
    Just Yellow -> Just Red
    Nothing -> Nothing

updateMousePosition :: (Float,Float) -> Game -> Game
updateMousePosition (mouseXPosition,_) game =
  game { columnUnderMouse = Just $ targetColumn mouseXPosition}

targetColumn :: Float -> Column
targetColumn mouseXPosition =
  floor $
  (fromIntegral screenWidth / 2 + mouseXPosition) / slotWidth


testFor4InALine
  :: Grid
  -> ((Int,Int),Slot)
  -> [(Int,Int)]
testFor4InALine grid x =
  concatMap fromJust $
  filter isJust $
  map (\test -> test x) tests
  where
    tests =
      [ testHorizontal grid
      , testVertical grid
      , testDiagonalTopLeft grid
      , testDiagonalTopRight grid
      ]

maybeValid
  :: [(i,i)]
  -> Maybe [(i,i)]
maybeValid path =
  if length path < 4
     then Nothing
     else Just path

walkThroughGrid
  :: (Int -> Int)
  -> (Int -> Int)
  -> Grid
  -> ((Int,Int),Slot)
  -> [(Int,Int)]
walkThroughGrid colStep rowStep (Grid grid) (pos@(col,row),slot)
      | col `notElem` [0..6] = []
      | row `notElem` [0..5] = []
      | grid ! pos == slot =
        pos : walkThroughGrid colStep rowStep
                              (Grid grid) ((colStep col, rowStep row),slot)
      | otherwise = []

testHorizontal
  :: Grid
  -> ((Int,Int),Slot)
  -> Maybe [(Int,Int)]
testHorizontal grid ((col,row),slot) = maybeValid path
  where
    path = walkLeft ((col-1,row),slot)
      ++ [(col,row)]
      ++ walkRight ((col+1,row),slot)
    walkLeft = walkThroughGrid (subtract 1) id grid
    walkRight = walkThroughGrid (+ 1) id grid

testVertical
  :: Grid
  -> ((Int,Int),Slot)
  -> Maybe [(Int,Int)]
testVertical grid ((col,row),slot) = maybeValid path
  where
    path = walkDown ((col,row),slot)
    walkDown = walkThroughGrid id (subtract 1) grid

testDiagonalTopLeft
  :: Grid
  -> ((Int,Int),Slot)
  -> Maybe [(Int,Int)]
testDiagonalTopLeft grid ((col,row),slot) = maybeValid path
  where
    path = walkUpLeft ((col-1,row+1),slot)
      ++ [(col,row)]
      ++ walkDownRight ((col+1,row-1),slot)
    walkUpLeft = walkThroughGrid (subtract 1) (+1) grid 
    walkDownRight = walkThroughGrid (+1) (subtract 1) grid 

testDiagonalTopRight
  :: Grid
  -> ((Int,Int),Slot)
  -> Maybe [(Int,Int)]
testDiagonalTopRight grid ((col,row),slot) = maybeValid path
  where
    path = walkUpRight ((col+1,row+1),slot) 
      ++ [(col,row)]
      ++ walkDownLeft ((col-1,row-1),slot)
    walkUpRight = walkThroughGrid (+1) (+1) grid 
    walkDownLeft = walkThroughGrid (subtract 1) (subtract 1) grid 

