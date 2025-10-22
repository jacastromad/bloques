-- Game/Logic.hs
module Game.Logic
  ( step
  , moveL
  , moveR
  , softD
  , hardD
  , rotL
  , rotR
  , nextShape  --TODO: remove (used in main to setup world)
  , spawn      --TODO: remove this one too
  ) where

import Game.Types
import Game.Shapes
import qualified Data.Vector as V
import Data.Word (Word64)
import Data.Bits (xor, shiftR, shiftL)


-- | Single simulation step.
step :: Float -> World -> World
step _ w@World{ state = Over } = w
step i w@World{ board = b, piece = p, level = l, fallAcc = s, state = st }
  | st /= Running = w
  | s-i > 0 = w{fallAcc = s-i}
  | canPlace b (translate (0, 1) p) = w{piece = translate (0, 1) p
                                       , fallAcc = 0.5-(0.05*fromIntegral l)}
  | otherwise = lockAndAdvance w

-- Move left
moveL :: World -> World
moveL w@World{ board=b, piece=p } = if canPlace b q then w{ piece=q } else w
  where q = translate (-1,0) p

-- Move right
moveR :: World -> World
moveR w@World{ board=b, piece=p } = if canPlace b q then w{ piece=q } else w
  where q = translate (1,0) p

-- Move down
softD :: World -> World
softD w@World{ board=b, piece=p } = if canPlace b q then w{ piece=q } else lockAndAdvance w
  where q = translate (0,1) p

-- Drop down
hardD :: World -> World
hardD w@World{ board=b, piece=p } = lockAndAdvance w{ piece=q }
  where q = hardDrop b p

-- Rotate clockwise
rotR :: World -> World
rotR w@World{ board=b, piece=p } = if canPlace b q then w{ piece=q } else w
  where q = p{ rot = rotateR (rot p) }

-- Rotate counter-clockwise
rotL :: World -> World
rotL w@World{ board=b, piece=p } = if canPlace b q then w{ piece=q } else w
  where q = p{ rot = rotateL (rot p) }


-- Internal helpers
--
-- | Lock the current piece into the board, clear lines, update score/level,
--   and spawn the next piece from the queue. Ends the game if spawn fails.
-- | Advance the game after the falling piece can no longer move down:
--   1) lock current piece into the board
--   2) clear full lines and update score/level counters
--   3) promote preview 'next' to active 'piece'
--   4) generate a new preview from the RNG seed and spawn it
--   5) game over if the promoted piece cannot be placed
lockAndAdvance :: World -> World
lockAndAdvance w@World{ board=b, piece=p, next=np, score=sc, level=lvl, cleared=cl, seed=s } =
  let -- 1) lock
      b1 = stamp b p
      -- 2) clear lines + scoring
      (b2, nCleared) = clearLines b1
      sc'  = sc + nCleared*(lvl+1)
      cl'  = cl + nCleared
      lvl' = cl' `div` 10
      -- 3) promote preview piece to active
      promoted = np
      -- 4) generate next preview from seed and spawn it
      (nShape, s') = nextShape s
      nextP        = spawn nShape b2
  in
    -- 5) if promoted cannot be placed, game over
    if canPlace b2 promoted
       then w { board = b2
              , piece = promoted
              , next  = nextP
              , score = sc'
              , cleared = cl'
              , level = lvl'
              , seed = s' }
       else w { board = b2
              , state = Over
              , score = sc'
              , cleared = cl'
              , level = lvl'
              , seed = s' }

-- | Drop the piece straight down until just before collision.
hardDrop :: Board -> Piece -> Piece
hardDrop b p = if canPlace b p' then hardDrop b p' else p
  where p' = translate (0, 1) p

-- | Pure RNG: advance a 64-bit xorshift generator.
--   https://en.wikipedia.org/wiki/Xorshift
stepRNG :: Word64 -> Word64
stepRNG x0 = x3 * 2685821657736338717
  where x1 = x0 `xor` (x0 `shiftR` 12)
        x2 = x1 `xor` (x1 `shiftL` 25)
        x3 = x2 `xor` (x2 `shiftR` 27)

-- | Draw a Shape from seed and return the updated seed.
--   Keeps your 'World' field as 'Int' by converting at the edges.
nextShape :: Int -> (Shape, Int)
nextShape s = (toEnum i, fromIntegral w1)
  where w0 = fromIntegral s :: Word64
        w1 = stepRNG w0
        i  = fromIntegral (w1 `mod` 7) :: Int  -- 0..6

-- | Write the piece blocks into the board with a per-shape color.
stamp :: Board -> Piece -> Board
stamp b p = foldl (\acc c -> write acc c (Block (colorOf (shape p)))) b (cellsOf p)

-- | Spawn piece centered near the top with rotation R0.
spawn :: Shape -> Board -> Piece
spawn s (Board w _ _) = Piece s (w `div` 2 - 2, 0) R0 (colorOf s)


-- | True when every block of the piece lies in-bounds and on an empty cell.
canPlace :: Board -> Piece -> Bool
canPlace b p = all (inBounds b) cs && all (free b) cs
  where cs = cellsOf p

-- | Clear all full lines. Returns the new board and the number cleared.
clearLines :: Board -> (Board, Int)
clearLines (Board w h v) = (Board w h newVec, nFullYs)
  where row    y = [y*w + x | x <- [0..w-1]]
        isFull y = all (\i -> v V.! i /= Empty) (row y)
        fullYs   = [y | y <- [0..h-1], isFull y]
        keepYs   = [y | y <- [0..h-1], not (isFull y)]
        keptRows = concatMap row keepYs
        keptVec  = V.backpermute v (V.fromList keptRows)
        nFullYs  = length fullYs
        newVec   = V.replicate (nFullYs*w) Empty V.++ keptVec

