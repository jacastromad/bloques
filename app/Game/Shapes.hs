-- Game/Shapes.hs
module Game.Shapes (cellsOf, colorOf, shapeCells) where

import Game.Types


-- | All board coordinates occupied by a piece at its current pose.
--   Uses a simple 4×4 canonical layout per SRS-like tables.
cellsOf :: Piece -> [Pos]
cellsOf (Piece s (px, py) r _) = [(px + x, py + y) | (x, y) <- shapeCells s r]

-- | Color mapping per shape.
colorOf :: Shape -> BColor
colorOf I = CCyn
colorOf O = CYel
colorOf T = CMag
colorOf S = CGrn
colorOf Z = CRed
colorOf J = CBlu
colorOf L = COrg

-- | Block coordinates per shape and rotation.
--   Coordinates are relative to the piece's top-left corner.
shapeCells :: Shape -> Rotation -> [Pos]
shapeCells I R0   = [(0,1),(1,1),(2,1),(3,1)]
shapeCells I R90  = [(2,0),(2,1),(2,2),(2,3)]
shapeCells I R180 = shapeCells I R0
shapeCells I R270 = shapeCells I R90

shapeCells O _    = [(1,0),(2,0),(1,1),(2,1)]

shapeCells T R0   = [(1,0),(0,1),(1,1),(2,1)]
shapeCells T R90  = [(1,0),(1,1),(2,1),(1,2)]
shapeCells T R180 = [(0,1),(1,1),(2,1),(1,2)]
shapeCells T R270 = [(1,0),(0,1),(1,1),(1,2)]

shapeCells S R0   = [(1,0),(2,0),(0,1),(1,1)]
shapeCells S R90  = [(1,0),(1,1),(2,1),(2,2)]
shapeCells S R180 = shapeCells S R0
shapeCells S R270 = shapeCells S R90

shapeCells Z R0   = [(0,0),(1,0),(1,1),(2,1)]
shapeCells Z R90  = [(2,0),(1,1),(2,1),(1,2)]
shapeCells Z R180 = shapeCells Z R0
shapeCells Z R270 = shapeCells Z R90

shapeCells J R0   = [(0,0),(0,1),(1,1),(2,1)]
shapeCells J R90  = [(1,0),(2,0),(1,1),(1,2)]
shapeCells J R180 = [(0,1),(1,1),(2,1),(2,2)]
shapeCells J R270 = [(1,0),(1,1),(0,2),(1,2)]

shapeCells L R0   = [(2,0),(0,1),(1,1),(2,1)]
shapeCells L R90  = [(1,0),(1,1),(1,2),(2,2)]
shapeCells L R180 = [(0,1),(1,1),(2,1),(0,2)]
shapeCells L R270 = [(0,0),(1,0),(1,1),(1,2)]


