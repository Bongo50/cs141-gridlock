module Gridlock.Types where

import Data.Map (Map)
import Data.Set (Set)

--------------------------------------------------------------------------------

-- | A player is identified by their name.
type Player = String

{- | A coordinate is a pair of integers.
  The first one is the x-coordinate (column number),
  and the second one is the y-coordinate (row number).
-}
type Coord = (Int, Int)

{- | A grid is a 2D array of cells.
  The grid is represented as a Map from Coords (a pair of Ints) to Cells.
-}
data Grid = Grid
    { width :: Int
    -- ^ The number of columns in the grid
    , height :: Int
    -- ^ The number of rows in the grid
    , rep :: Map Coord Cell
    -- ^ The internal representation of the grid
    }
    deriving (Eq, Ord, Read, Show)

-- | A cell is either empty or contains a colour.
data Cell = Empty | Filled Colour
    deriving (Eq, Ord, Read, Show)

-- | The set of colours is fixed in advance and corresponds to the six ANSI base terminal colours (white and black excluded).
data Colour = Red | Green | Yellow | Blue | Magenta | Cyan
    deriving (Eq, Ord, Bounded, Enum, Read, Show)

-- | A GameRecord is a complete record of a full game.
data GameRecord = GameRecord
    { players :: [Player]
    -- ^ The list of players in the game.
    , grids :: [Grid]
    -- ^ The list of grids as the game evolved.
    , colours :: Set Colour
    -- ^ The list of colours in the game.
    }
    deriving (Eq, Ord, Read, Show)
