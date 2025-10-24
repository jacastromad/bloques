-- Game/Render.hs
-- Pure rendering of the game state using Gloss.
module Game.Render (render) where

import Graphics.Gloss
import qualified Data.Vector as V

import Game.Assets (Assets(..))
import Game.Types
import Game.Shapes (cellsOf, colorOf, shapeCells)


-- | Render the full frame: settled board + falling piece.
render :: Assets -> World -> Picture
render a World{ board = b, piece = p, next = n, score = sc, level = lv
              , cleared = cl } =
  Pictures [ renderBg a
           , renderGrid a b
           , renderBoard a b
           , renderPiece a b p
           , renderNext a b n
           , renderHUD a b sc lv cl ]

-- | Draw thin grid lines over the board area.
renderGrid :: Assets -> Board -> Picture
renderGrid Assets{cellSize=s} (Board w h _) = Color (withAlpha 0.25 white) $
                                              Pictures (bg ++ vs ++ hs)
  where
    (x0,y0) = (-(cw / 2),  ch/2)        -- top-left corner
    cw      = fromIntegral w * s
    ch      = fromIntegral h * s
    bg = [Color black (rectangleSolid (cw+8) (ch+8))]
    vs = [Line [(x0 + i*s, y0), (x0 + i*s, y0 - ch)] | i <- [0..fromIntegral w]]
    hs = [Line [(x0, y0 - j*s), (x0 + cw, y0 - j*s)] | j <- [0..fromIntegral h]]

-- | Render all settled cells on the board.
renderBoard :: Assets -> Board -> Picture
renderBoard a b@(Board w h vec) =
  Pictures
    [ drawCell a b (x,y) col
    | y <- [0 .. h-1]
    , x <- [0 .. w-1]
    , let c = vec V.! (y*w + x)
    , col <- cellColor c ]
  where
    cellColor Empty        = []
    cellColor (Block bc)   = [bc]

-- | Render the current falling piece (uses the shape palette).
renderPiece :: Assets -> Board -> Piece -> Picture
renderPiece a b p = Pictures [drawCell a b c bc | c <- cellsOf p]
  where bc = colorOf (shape p)

-- | Render next piece on the right of the board
renderNext :: Assets -> Board -> Piece -> Picture
renderNext a@(Assets{cellSize=s}) b@(Board w h _) (Piece sh _ r _) =
  Pictures
    [ Translate cx cy $ Color black $ rectangleSolid boxW boxH
    , Translate dx dy $  -- because drawCell uses board coords
        Pictures [ drawCell a b (x, y) (colorOf sh)
                 | (x, y) <- shapeCells sh r ] ]
  where
    -- board geometry
    (cw, ch) = (fromIntegral w * s, fromIntegral h * s)
    (x0, y0) = toScreen s b (0, 0)       -- world coords of board cell (0,0)
    -- preview geometry
    (ax, ay) = (cw/2 + s*3, ch/2 - s*6)  -- world coords for preview cell (0,0)
    pad      = s * 0.5
    (boxW, boxH) = (4*s + 2*pad, 3*s + 2*pad)
    (cx, cy) = (ax + 1.5*s, ay - 1*s)   -- background rect center
    (dx, dy) = (ax - x0, ay - y0)       -- delta from board to preview

-- | Render HUD (score, level, cleared) on the left
renderHUD :: Assets -> Board -> Int -> Int -> Int -> Picture
renderHUD Assets{cellSize=s} (Board w h _) sc lv cl =
  Pictures
    [ Translate (ax+50) (top-45) $ Color black (rectangleSolid (cw/2) (ch/3)) 
    , textline 0 ("SCORE: "  ++ show sc)
    , textline 1 ("LEVEL: "  ++ show lv)
    , textline 2 ("LINES: "  ++ show cl) ]
  where cw = fromIntegral w * s
        ch = fromIntegral h * s
        margin = s * 1.0
        ax = -(cw/2) - margin - s*6
        top = ch/2 - s*6
        textline dy str = Translate ax (top - dy*s*2) $ Scale 0.12 0.12 $ Color white $ Text str

-- | Render background centered behind board
renderBg :: Assets -> Picture
renderBg Assets{background = b} = case b of
                                    Just pic -> pic
                                    _        -> Blank

-- | Draw a single block at a board coordinate with the given logical color.
drawCell :: Assets -> Board -> Pos -> BColor -> Picture
drawCell Assets{ cellSize = s, glossColor = gc } b p bc =
  Translate sx sy $ Pictures
    [ Color border $ rectangleSolid s s
    , Color base $ rectangleSolid (s*0.8) (s*0.8) ]
  where (sx, sy) = toScreen s b p
        base     = gc bc
        border   = mixColors 0.7 0.3 base black -- or: darken 0.2 base

-- | Convert board coords (x, y) to Gloss coords (pixels), centered at (0, 0).
--   Board (0, 0) is top-left. Gloss y+ is up, so we flip Y.
toScreen :: Float -> Board -> Pos -> (Float, Float)
toScreen s (Board w h _) (x,y) = (xf, yf)
  where cw = fromIntegral w * s
        ch = fromIntegral h * s
        x0 = -cw/2 + s/2
        y0 =  ch/2 - s/2
        xf = x0 + fromIntegral x * s
        yf = y0 - fromIntegral y * s


