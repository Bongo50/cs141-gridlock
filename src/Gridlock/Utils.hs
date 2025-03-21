module Gridlock.Utils where
import Gridlock.Types (Player, GameRecord(players, grids))

getPlayerInNMoves :: Int -> GameRecord -> Player
getPlayerInNMoves n gr = ps !! ((l + n) `mod` 2)
    where
        ps = players gr
        l = length $ grids gr
