-- Game/Events.hs
-- Map Gloss input events to pure world updates.
module Game.Events (handle) where

import Graphics.Gloss.Interface.Pure.Game
import Game.Types
import Game.Logic


-- | Handle a single Gloss 'Event'.
--   Arrows: move/soft drop.  Z/X or Up: rotate.  Space: hard drop.  P: pause toggle.
handle :: Event -> World -> World
handle ev w =
  case ev of
    EventKey (SpecialKey KeyLeft)  Down _ _ -> whenRunning moveL w
    EventKey (SpecialKey KeyRight) Down _ _ -> whenRunning moveR w
    EventKey (SpecialKey KeyDown)  Down _ _ -> whenRunning softD w
    EventKey (SpecialKey KeyUp)    Down _ _ -> whenRunning rotL w
    EventKey (SpecialKey KeySpace) Down _ _ -> whenRunning hardD w
    EventKey (Char 'z')            Down _ _ -> whenRunning rotL w
    EventKey (Char 'x')            Down _ _ -> whenRunning rotR w
    EventKey (Char 'p')            Down _ _ -> togglePause w
    _                                       -> w

-- Only act when the game is running.
whenRunning :: (World -> World) -> World -> World
whenRunning f w = case state w of
  Running -> f w
  _       -> w

-- Toggle between Running and Paused.
togglePause :: World -> World
togglePause w = case state w of
  Running -> w{ state = Paused }
  Paused  -> w{ state = Running }
  Over    -> w
