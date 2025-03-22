module Gridlock.Utils where

import Gridlock.Types (Player, GameRecord(..), Colour)
import Data.Char (toLower)
import Data.Set (empty)

getPlayerInNMoves :: Int -> GameRecord -> Player
getPlayerInNMoves n gr = ps !! ((l + n) `mod` 2)
    where
        ps = players gr
        l = length $ grids gr

displayColour :: Colour -> String
displayColour = map toLower . show

emptyRecord :: GameRecord
emptyRecord = GameRecord
    { players = []
    , grids = []
    , colours = empty }
