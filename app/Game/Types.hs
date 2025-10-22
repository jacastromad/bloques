--Game/Types.hs
module Game.Types where

import qualified Data.Vector as V
import Data.Maybe (isJust)


-- | Logical color tags for blocks.
data BColor = CRed | CGrn | CBlu | CYel | CCyn | CMag | COrg
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | The seven usual shapes
data Shape = I | O | T | S | Z | J | L deriving (Eq, Ord, Enum, Bounded, Show)

-- | A cell on the board: empty or a solid block with a color tag.
data Cell = Empty | Block BColor deriving (Eq, Show)

-- | Playfield stored row-major as a flat vector.
data Board = Board { width :: !Int, height :: !Int, cells :: !(V.Vector Cell) }
  deriving Show

-- | An active piece with its shape, position (x, y), and rotation.
--   Position is in board cells with (0,0) at the top-left.
data Piece = Piece { shape :: Shape, pos :: (Int, Int), rot :: Rotation
                   , color :: BColor}
  deriving Show

-- | Rotation state, clockwise in 90° steps.
data Rotation = R0 | R90 | R180 | R270 deriving (Eq, Enum, Bounded, Show)

-- | Game status
data GameStatus = Running | Paused | Over deriving (Eq, Show)

-- | Full game state with current piece, next piece and scores.
data World = World { state :: GameStatus  -- Running, Paused or Over
                   , board :: Board       -- board and fixed blocks
                   , piece :: Piece       -- current falling piece
                   , next :: Piece        -- next piece
                   , fallAcc :: Float     -- seconds to next step
                   , score :: Int         -- score
                   , level :: Int         -- level
                   , cleared :: Int       -- total cleared lines
                   , seed :: Int          -- random number generator seed
                   } deriving Show

-- | 2D integer coordinate (x,y) in board space.
type Pos = (Int, Int)


-- | Linearize a board coordinate to a vector index if in bounds.
--   Returns 'Nothing' when (x,y) is outside the board.
ix :: Board -> Pos -> Maybe Int
ix (Board w h _) (x, y)
  | x < 0 || y < 0 || x >= w || y >= h = Nothing
  | otherwise                          = Just (y*w + x)
{-# INLINE ix #-}

-- | True if the coordinate lies within the board rectangle.
inBounds :: Board -> Pos -> Bool
inBounds b c = isJust (ix b c)
{-# INLINE inBounds #-}

-- | Read a cell at a coordinate. 'Nothing' if out of bounds.
at :: Board -> Pos -> Maybe Cell
at (Board w h v) (x, y)
  | x < 0 || y < 0 || x >= w || y >= h = Nothing
  | otherwise                          = Just (v V.! (y*w + x))

-- | Immutable write: update one cell, returning a new board.
--   Out-of-bounds writes are ignored and return the original board.
write :: Board -> Pos -> Cell -> Board
write b@(Board w h v) p a =
  case ix b p of
    Nothing -> b
    Just i  -> Board w h (v V.// [(i, a)])

-- | True if the cell at the coordinate is Empty.
--   Out-of-bounds counts as not occupied.
free :: Board -> Pos -> Bool
free b p = b `at` p == Just Empty
{-# INLINE free #-}


-- | Construct an empty board filled with 'Empty' cells.
emptyBoard :: Int -> Int -> Board
emptyBoard w h = Board w h (V.replicate (w*h) Empty)

-- | All coordinates of the board in row-major order.
coords :: Board -> [Pos]
coords (Board w h _) = [ (x,y) | y <- [0..h-1], x <- [0..w-1] ]

-- | Rotate 90° clockwise. Wraps at R270 -> R0.
rotateR :: Rotation -> Rotation
rotateR r = if r == maxBound then minBound else succ r
{-# INLINE rotateR #-}

-- | Rotate 90° counter-clockwise. Wraps at R0 -> R270.
rotateL :: Rotation -> Rotation
rotateL r = if r == minBound then maxBound else pred r
{-# INLINE rotateL #-}

-- | Translate a piece by (dx, dy) in board cells.
translate :: (Int, Int) -> Piece -> Piece
translate (dx, dy) p@Piece{pos=(x, y)} = p{pos = (x+dx, y+dy)}
{-# INLINE translate #-}


