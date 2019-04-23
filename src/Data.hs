module Data where

import Data.List
import Data.Array

---------
  -- Constants
---------

screenWidth = 640 :: Int
screenHeight = 480 :: Int

slotWidth = fromIntegral screenWidth / 7 :: Float
slotHeight = fromIntegral screenHeight / 7 :: Float

gridWidth = slotWidth * 7 :: Float
gridHeight = slotHeight * 6 :: Float

slotRadius = (slotHeight / 2) * 0.875 :: Float

--------
  -- Data
--------

data Player = Red | Yellow 
instance Show Player where
  show Red = "R"
  show Yellow = "Y"
instance Eq Player where
  Red == Red = True
  Yellow == Yellow = True
  _ == _ = False
  (/=) x = not . (x==)

newtype Slot = Slot {content :: (Maybe Player)} deriving (Eq)
instance Show Slot where
  show x = 
    case (content x) of
      Nothing -> "."
      Just x -> show x

-- [ [ColumnBegin, .. , ColumnEnd] ], because it's easier to drop the drisks
newtype Grid = Grid (Array (Int,Int) Slot) deriving (Eq)
instance Show Grid where
  show (Grid xs) = unlines $ 
                   map (unwords . map (show . snd)) $ 
                   groupBy  (\((x,_),_) ((y,_),_) -> x == y) $ 
                   assocs xs
  --show' (Grid xs) = unlines $ map (unwords . map (show . snd)) $ 
  --                groupBy  (\((_,x),_) ((_,y),_) -> x == y) $ 
  --                sortBy (\((_,x),_) ((_,y),_) -> compare y x) $ assocs xs

type Column = Int
type Row = Int
data GameState =
  DropCoin |
  Gameover [(Int,Int)]
  deriving (Show,Eq)

data Game = Game 
  { gameGrid :: Grid
  , gameState :: GameState
  , currentPlayer :: Maybe Player
  , columnUnderMouse :: Maybe Column
  } deriving (Show,Eq)


