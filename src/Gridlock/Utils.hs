module Gridlock.Utils where

import Gridlock.Types (Player, GameRecord(..), Colour)
import Data.Char (toLower)
import Data.Set (empty)

{- | Takes an @Int@, n, and a @GameRecord@, gs, and finds the player who's turn
    it would be in n moves from the last move in gs.-}
getPlayerInNMoves :: Int -> GameRecord -> Player
getPlayerInNMoves n gr = ps !! ((l + n) `mod` 2)
    where
        ps = players gr
        l = length $ grids gr

-- Returns a lower-case @String@ representation of the supplied @Colour@.
displayColour :: Colour -> String
displayColour = map toLower . show

-- An empty @GameRecord@, used when initiating the parse.
emptyRecord :: GameRecord
emptyRecord = GameRecord
    { players = []
    , grids = []
    , colours = empty }

-- | The file extension that Gridlock files should use.
fileExtension :: String
fileExtension = ".gridlock"