{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Gridlock.Parser where

import Text.Megaparsec
    ( MonadParsec(try), (<|>), Parsec, manyTill, choice, many, some )
import Text.Megaparsec.Char ( string, newline, alphaNumChar, char )
import Text.Megaparsec.Char.Lexer ( decimal )

--import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Void ( Void )
--import Gridlock.DrawGrid ()
import Gridlock.Types
    ( GameRecord(..),
      Colour(..),
      Cell(Filled, Empty),
      Grid(Grid, rep, width, height),
      Player )

type Parser = Parsec Void String

{- | Parse a game into a GameRecord.

To run this function (either as part of your application, or for testing, you can use one of the parse* functions from the Megaparsec library, such as "parse" or "parseTest".)
-}
parseGame :: Parser GameRecord
parseGame = do
    sequence_ [string "GRIDLOCK", some newline]
    players <- parsePlayers
    some newline
    (width, height) <- parseSize
    some newline
    colours <- parseColours
    some newline
    let startingRecord = GameRecord { players = players
                                    , grids = [Grid
                                        { width = width
                                        , height = height
                                        , rep = Map.fromList $ [((x,y), Empty) | x <- [0 .. width-1], y <- [0 .. height-1]] }]
                                    , colours = colours }
    finalRecord <- try (parseMoves startingRecord) <|> pure startingRecord
    let winner = players !! (length (grids finalRecord) `mod` 2)
    let loser = players !! ((length (grids finalRecord) - 1) `mod` 2)
    string $ loser ++ " cannot move."
    if canMove finalRecord then
        fail $ "Invalid loss as " ++ loser ++ " can move."
    else do
        some newline
        string $ winner ++ " wins!"
        pure finalRecord

parseMove :: GameRecord -> Parser GameRecord
parseMove oldRecord = do
    let gamePlayers = players oldRecord
    let gameColours = colours oldRecord
    let oldGrid = grids oldRecord !! (length (grids oldRecord) - 1)
    let oldRep = rep oldGrid
    let prevPlayer = gamePlayers !! (length (grids oldRecord) `mod` 2)
    player <- manyTill alphaNumChar $ string " plays "
    if player `notElem` gamePlayers then
        fail $ player ++ "is an invalid player name."
    else if player == prevPlayer then
        fail $ player ++ " is moving out of turn."
    else do
        moveColour <- colour
        if moveColour `notElem` gameColours then
            fail $ show moveColour ++ " is an invalid colour."
        else do
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
                pure $ oldRecord {grids = grids oldRecord ++ [oldGrid {rep = Map.adjust (const $ Filled moveColour) (x,y) oldRep}]}

parseMoves :: GameRecord -> Parser GameRecord
parseMoves x = do
    y <- parseMove x
    some newline
    try (parseMoves y) <|> pure y

{-  canMove takes a GameRecord and checks if there is a valid move than can be made from it. This is in order to
    determine if a gridlock file reports the end of the game too early. -}
canMove :: GameRecord -> Bool
canMove record =
    let
        grid = grids record !! (length (grids record) - 1)
        gridR = rep grid
    in
        or [ gridR Map.!? (x,y) == Just Empty && not (any ((== (Just $ Filled c)) . (gridR Map.!?)) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]) | x <- [0 .. width grid - 1], y <- [0 .. height grid -1], c <- Set.toList $ colours record]

parsePlayers :: Parser [Player]
parsePlayers = do
    string "Players: "
    p1 <- manyTill alphaNumChar $ char ','
    char ' '
    p2 <- manyTill alphaNumChar $ char '.'
    pure [p1, p2]

parseSize :: Parser (Int, Int)
parseSize = do
    string "Grid: "
    w <- decimal
    char 'x'
    h <- decimal
    char '.'
    pure (w, h)

colour :: Parser Colour
colour = choice
    [ string "red" >> pure Red
    , string "green" >> pure Green
    , string "yellow" >> pure Yellow
    , string "blue" >> pure Blue
    , string "magenta" >> pure Magenta
    , string "cyan" >> pure Cyan
    ]

parseColours :: Parser (Set Colour)
parseColours = do
    string "Colours: "
    c1 <- colour
    colours <- many $ do
        string ", "
        colour
    char '.'
    pure $ Set.fromList $ c1 : colours

parseCoords :: Parser (Int, Int)
parseCoords = do
    char '('
    x <- decimal
    char ','
    y <- decimal
    char ')'
    pure (x,y)
