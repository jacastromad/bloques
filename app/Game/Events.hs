-- Game/Events.hs
-- Map Gloss input events to pure world updates.
module Game.Events (handle) where

import Graphics.Gloss.Interface.Pure.Game
import Game.Types
import Game.Logic
import qualified Data.Map.Strict as M


-- TODO: refactor handle

-- | Handle a single Gloss 'Event'.
--   Arrows: move/soft drop.  Z/X or Up: rotate.  Space: hard drop.  P: pause toggle.
handle :: Event -> World -> World
handle (EventKey (Char 'p') Down _ _) w
  | state w /= Over = togglePause w
handle ev w@World { keys = ks, state = Running } =
  case ev of
    EventKey (SpecialKey KeyLeft)  Down _ _ -> moveL w { keys = M.insert (SpecialKey KeyLeft) 0.15 ks }
    EventKey (SpecialKey KeyLeft)  Up   _ _ -> w { keys = M.delete (SpecialKey KeyLeft) ks }
    EventKey (SpecialKey KeyRight) Down _ _ -> moveR w { keys = M.insert (SpecialKey KeyRight) 0.15 ks }
    EventKey (SpecialKey KeyRight) Up   _ _ -> w { keys = M.delete (SpecialKey KeyRight) ks }
    EventKey (SpecialKey KeyDown)  Down _ _ -> softD w { keys = M.insert (SpecialKey KeyDown) 0.15 ks }
    EventKey (SpecialKey KeyDown)  Up   _ _ -> w { keys = M.delete (SpecialKey KeyDown) ks }
    EventKey (SpecialKey KeyUp)    Down _ _ -> rotL w { keys = M.insert (SpecialKey KeyUp) 0.15 ks }
    EventKey (SpecialKey KeyUp)    Up   _ _ -> w { keys = M.delete (SpecialKey KeyUp) ks }
    EventKey (SpecialKey KeySpace) Down _ _ -> hardD w { keys = M.insert (SpecialKey KeySpace) 0.15 ks }
    EventKey (SpecialKey KeySpace) Up   _ _ -> w { keys = M.delete (SpecialKey KeySpace) ks }
    EventKey (Char 'z')            Down _ _ -> rotL w { keys = M.insert (Char 'z') 0.15 ks }
    EventKey (Char 'z')            Up   _ _ -> w { keys = M.delete (Char 'z') ks }
    EventKey (Char 'x')            Down _ _ -> rotR w { keys = M.insert (Char 'x') 0.15 ks }
    EventKey (Char 'x')            Up   _ _ -> w { keys = M.delete (Char 'x') ks }
    _                                       -> w
handle _ w = w

-- Toggle between Running and Paused.
togglePause :: World -> World
togglePause w = case state w of
  Running -> w{ state = Paused }
  Paused  -> w{ state = Running }
  Over    -> w

