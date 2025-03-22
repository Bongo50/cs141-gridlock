module Gridlock.Utils where

import Gridlock.Types ( GameRecord(..), Colour, Player )
import Data.Char ( toLower )
import Data.Set ( empty )

{- | Takes an @Int@, n, and a @GameRecord@, gs, and finds the player who's turn
    it would be in n moves from the last move in gs.-}
getPlayerInNMoves :: Int -> GameRecord -> Player
getPlayerInNMoves n GameRecord{players, grids} = players !! ((length grids + n) `mod` 2)

-- Returns a lower-case @String@ representation of the supplied @Colour@.
displayColour :: Colour -> String
displayColour = map toLower . show

-- An empty @GameRecord@, used when initialising the parse.
emptyRecord :: GameRecord
emptyRecord = GameRecord
    { players = []
    , grids = []
    , colours = empty }

-- | The file extension that Gridlock files should use.
fileExtension :: String
fileExtension = ".gridlock"