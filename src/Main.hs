module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data
import Game
import Graphics

import Data.Array

window :: Display
window = InWindow "Connect 4" (screenWidth, screenHeight) (0, 0)

background :: Color
background = black

main :: IO ()
main = play window background 30 initGame drawGame transformGame (const id)

