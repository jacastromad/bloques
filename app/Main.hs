-- Main.hs
-- Launches the game
module Main (main) where

import Graphics.Gloss
import Game.Assets (Assets(..), loadAssets)
import Game.Types
import Game.Render (render)
import Game.Logic (step, spawn, nextShape)
import Game.Events (handle)
import System.Random (randomIO)


-- | Launch the game
main :: IO ()
main = do
  bAssets <- loadAssets
  bgPic   <- loadBMP "./assets/background.bmp"
  s0      <- randomIO :: IO Int      -- random gen seed
  let assets    = bAssets { background = Just bgPic }
      brd       = emptyBoard 10 20   -- 10 columns x 20 rows
      (shA, s1) = nextShape s0
      p0        = spawn shA brd      -- current piece
      (shB, s2) = nextShape s1
      n0        = spawn shB brd      -- next piece
      w0 = World { state = Running, board = brd, piece = p0, fallAcc = 0.5
                 , next = n0, score = 0, level = 1, cleared = 0, seed = s2 }
      win   = InWindow "Bloques" (winW assets brd, winH assets brd) (100,100)
      fps   = 60
  play win black fps w0 (render assets) handle step


-- | Pixel size of the window matching the board.
winW :: Assets -> Board -> Int
winW Assets{cellSize=s} Board{width=w}  = 600 + round (fromIntegral w * s)

winH :: Assets -> Board -> Int
winH Assets{cellSize=s} Board{height=h} = 100 + round (fromIntegral h * s)

