{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
module Gridlock.Parser where

import Text.Megaparsec
    ( MonadParsec(try), ParsecT, manyTill, choice, many, some )
import Text.Megaparsec.Char ( string, newline, alphaNumChar, char )
import Text.Megaparsec.Char.Lexer ( decimal )

import Control.Monad.State.Lazy
    ( MonadState(put, get), MonadTrans(lift), State )

import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Void ( Void )
import Gridlock.Types
    ( GameRecord(..),
      Colour(..),
      Cell(Filled, Empty),
      Grid(Grid, rep, width, height),
      Player, displayColour )

type StatefulParser = ParsecT Void String (State [Grid])

{- | Parse a game into a GameRecord.

To run this function (either as part of your application, or for testing, you can use one of the parse* functions from the Megaparsec library, such as "parse" or "parseTest".)
-}
parseGame :: StatefulParser GameRecord
parseGame = do
    string "GRIDLOCK"
    some newline
    players <- parsePlayers
    some newline
    (width, height) <- parseSize
    some newline
    colours <- parseColours
    some newline

    let emptyGrid = [Grid
            { width = width
            , height = height
            , rep = Map.fromList $ [((x,y), Empty) | x <- [0 .. width-1], y <- [0 .. height-1]] }]
    put emptyGrid

    finalGrids <- parseMoves players colours
    let winner = players !! ((length finalGrids + 1) `mod` 2)
    let loser = players !! (length finalGrids `mod` 2)
    string $ loser ++ " cannot move."
    if canMove colours $ last finalGrids then
        fail $ "Invalid loss as " ++ loser ++ " can move." ++ "\n"
    else do
        some newline
        string $ winner ++ " wins!"
        pure GameRecord { players = players
                        , grids = finalGrids
                        , colours = colours }

canMove :: Set Colour -> Grid -> Bool
canMove colours grid =
    let
        gridR = rep grid
    in
        or [ gridR Map.!? (x,y) == Just Empty && not (any ((== (Just $ Filled c)) . (gridR Map.!?)) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]) | x <- [0 .. width grid - 1], y <- [0 .. height grid -1], c <- Set.toList colours]

parseOneMove :: [Player] -> Set Colour -> StatefulParser Grid
parseOneMove gamePlayers gameColours = do 
    prevGrids <- get
    let oldGrid = head prevGrids
    let oldRep = rep oldGrid
    let playerToPlay = gamePlayers !! ((length prevGrids + 1) `mod` 2)
    string playerToPlay
    string " plays "
    moveColour <- choice  $ map parseColour $ Set.toList gameColours
    string " at "
    (x,y) <- parseCoords
    if x < 0 || x >= width oldGrid || y < 0 || y >= height oldGrid then
        fail $ "Move at " ++ show (x,y) ++ " is invalid as it is out of bounds."
    else if oldRep Map.! (x,y) /= Empty then
        fail $ "Move at " ++ show (x,y) ++ " is invalid as this cell is already filled."
    else if any ((== (Just $ Filled moveColour)) . (oldRep Map.!?)) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)] then
        fail $ "Move at " ++ show (x,y) ++ " is invalid as an adjacent cell already has this colour."
    else do
        string "."
        let newGrid = oldGrid {rep = Map.adjust (const $ Filled moveColour) (x,y) oldRep}
        put $ newGrid : prevGrids
        some newline
        pure newGrid
    where
        parseColour :: Colour -> StatefulParser Colour
        parseColour c = string (displayColour c) >> pure c

        parseCoords :: StatefulParser (Int, Int)
        parseCoords = do
            char '('
            x <- decimal
            char ','
            y <- decimal
            char ')'
            lift $ pure (x,y)

parseMoves :: [Player] -> Set Colour -> StatefulParser [Grid]
parseMoves gamePlayers gameColours = many $ try $ parseOneMove gamePlayers gameColours

{-

s -> Parser 

-}

parsePlayers :: StatefulParser [Player]
parsePlayers = do
    string "Players: "
    p1 <- manyTill alphaNumChar $ char ','
    char ' '
    p2 <- manyTill alphaNumChar $ char '.'
    pure [p1, p2]

parseSize :: StatefulParser (Int, Int)
parseSize = do
    string "Grid: "
    w <- decimal
    char 'x'
    h <- decimal
    char '.'
    pure (w, h)

parseColours :: StatefulParser (Set Colour)
parseColours = do
    string "Colours: "
    c1 <- colour
    colours <- many $ do
        string ", "
        colour
    char '.'
    pure $ Set.fromList $ c1 : colours

    where
        colour :: StatefulParser Colour
        colour = choice
            [ string "red" >> pure Red
            , string "green" >> pure Green
            , string "yellow" >> pure Yellow
            , string "blue" >> pure Blue
            , string "magenta" >> pure Magenta
            , string "cyan" >> pure Cyan
            ]