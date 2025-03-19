{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
module Gridlock.Parser where

import Text.Megaparsec
    ( MonadParsec(try), ParsecT, manyTill, choice, many, some, optional )
import Text.Megaparsec.Char ( string, newline, alphaNumChar, char )
import Text.Megaparsec.Char.Lexer ( decimal )

import Control.Monad.State.Lazy
    ( MonadState(put, get), MonadTrans(lift), State, StateT )

import Control.Monad.Tardis

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

type TardisStateParser = ParsecT Void String (StateT GameRecord (Tardis (Int, Int) (Int, Int)))

tLift :: Tardis (Int, Int) (Int, Int) a -> ParsecT Void String (StateT GameRecord (Tardis (Int, Int) (Int, Int))) a
tLift = lift . lift

parseGameStrict :: TardisStateParser GameRecord
parseGameStrict = do
    string "GRIDLOCK"
    some newline
    players <- parsePlayers
    some newline
    (width, height) <- parseSize
    some newline
    colours <- parseColours
    some newline

    oldRecord <- get
    put oldRecord { grids = [Grid
        { width = width
        , height = height
        , rep = Map.fromList $ [((x,y), Empty) | x <- [0 .. width-1], y <- [0 .. height-1]] }]}

    many $ try parseMoveStrict

    finalRecord <- get
    let finalGrids = grids finalRecord

    let winner = players !! (length finalGrids `mod` 2)
    let loser = players !! ((length finalGrids + 1) `mod` 2)
    string $ loser ++ " cannot move."
    if canMove colours $ last finalGrids then
        fail $ "Invalid loss as " ++ loser ++ " can move."
    else do
        some newline
        string $ winner ++ " wins!"
        pure finalRecord

parseGameLoose :: TardisStateParser GameRecord
parseGameLoose = do
    optional $ string "GRIDLOCK"
    many newline
    optional parsePlayers
    many newline
    maybeWidthHeight <- optional parseSize
    (width, height) <- case maybeWidthHeight of
        Just (x,y) -> pure (x, y)
        Nothing -> tLift getFuture
    many newline
    optional parseColours
    many newline

    oldRecord <- get
    put oldRecord { grids = [Grid
        { width = width
        , height = height
        , rep = Map.fromList $ [((x,y), Empty) | x <- [0 .. width-1], y <- [0 .. height-1]] }]}

    many $ try parseMoveLoose

    (bigX, bigY) <- tLift getPast
    tLift $ sendPast (bigX, bigY)

    finalRecord <- get
    --fail $ show finalRecord
    let finalGrids = grids finalRecord
    let finalPlayers = players finalRecord
    let finalColours = colours finalRecord

    let winner = finalPlayers !! (length finalGrids `mod` 2)
    let loser = finalPlayers !! ((length finalGrids + 1) `mod` 2)
    optional $ string $ loser ++ " cannot move."
    if canMove finalColours $ last finalGrids then
        fail $ "Invalid loss as " ++ loser ++ " can move."
    else do
        optional $ some newline
        optional $ string $ winner ++ " wins!"
        pure finalRecord

parseMoveLoose :: TardisStateParser Grid
parseMoveLoose = do
    oldRecord <- get
    let prevGrids = reverse $ grids oldRecord
    let oldGrid = head prevGrids
    let oldRep = rep oldGrid
    let oldPlayers = players oldRecord
    let oldColours = colours oldRecord

    player <- manyTill alphaNumChar $  string " plays "
    let gamePlayers =
            if length oldPlayers < 2 then
                reverse $ player : oldPlayers
            else oldPlayers
    if player `notElem` gamePlayers then
        fail $ player ++ " is not a valid player."
    else if gamePlayers !! ((length prevGrids + 1) `mod` 2) /= player then
        fail $ player ++ " is playing out of turn."
    else do

        moveColour <- choice $ map parseColour [Red .. Cyan]
        let gameColours = Set.insert moveColour oldColours

        string " at "
        (x,y) <- parseCoords
        tLift $ modifyForwards (\(x',y') -> (max x' x, max y' y))
        if x < 0 || x >= width oldGrid || y < 0 || y >= height oldGrid then
            fail $ "Move at " ++ show (x,y) ++ " is invalid as it is out of bounds."
        else if oldRep Map.! (x,y) /= Empty then
            fail $ "Move at " ++ show (x,y) ++ " is invalid as this cell is already filled."
        else if any ((== (Just $ Filled moveColour)) . (oldRep Map.!?)) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)] then
            fail $ "Move at " ++ show (x,y) ++ " is invalid as an adjacent cell already has this colour."
        else do
            string "."
            let newGrid = oldGrid {rep = Map.adjust (const $ Filled moveColour) (x,y) oldRep}
            put $ GameRecord
                { players = gamePlayers
                , grids = reverse $ newGrid : prevGrids
                , colours = gameColours }
            many newline
            pure newGrid
    where
        parseColour :: Colour -> TardisStateParser Colour
        parseColour c = string (displayColour c) >> pure c

        parseCoords :: TardisStateParser (Int, Int)
        parseCoords = do
            char '('
            x <- decimal
            char ','
            y <- decimal
            char ')'
            pure (x,y)

parseMoveStrict :: TardisStateParser Grid
parseMoveStrict = do
    oldRecord <- get
    let prevGrids = reverse $ grids oldRecord
    let oldGrid = head prevGrids
    let oldRep = rep oldGrid
    let gamePlayers = players oldRecord
    let gameColours = colours oldRecord
    let playerToPlay = gamePlayers !! ((length prevGrids + 1) `mod` 2)
    string playerToPlay
    string " plays "
    moveColour <- choice $ map parseColour $ Set.toList gameColours
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
        put $ oldRecord { grids = reverse $ newGrid : prevGrids }
        some newline
        pure newGrid
    where
        parseColour :: Colour -> TardisStateParser Colour
        parseColour c = string (displayColour c) >> pure c

        parseCoords :: TardisStateParser (Int, Int)
        parseCoords = do
            char '('
            x <- decimal
            char ','
            y <- decimal
            char ')'
            pure (x,y)

parsePlayers :: TardisStateParser [Player]
parsePlayers = do
    string "Players: "
    p1 <- manyTill alphaNumChar $ char ','
    char ' '
    p2 <- manyTill alphaNumChar $ char '.'
    oldRecord <- get
    put oldRecord { players = [p1,p2] }
    pure [p1, p2]

parseSize :: TardisStateParser (Int, Int)
parseSize = do
    string "Grid: "
    w <- decimal
    char 'x'
    h <- decimal
    char '.'
    pure (w, h)

parseColours :: TardisStateParser (Set Colour)
parseColours = do
    string "Colours: "
    c1 <- colour
    colours <- many $ do
        string ", "
        colour
    char '.'

    let coloursSet = Set.fromList $ c1 : colours
    oldRecord <- get
    put oldRecord { colours = coloursSet }
    pure coloursSet

    where
        colour :: TardisStateParser Colour
        colour = choice
            [ string "red" >> pure Red
            , string "green" >> pure Green
            , string "yellow" >> pure Yellow
            , string "blue" >> pure Blue
            , string "magenta" >> pure Magenta
            , string "cyan" >> pure Cyan
            ]

canMove :: Set Colour -> Grid -> Bool
canMove colours grid =
    let
        gridR = rep grid
    in
        or [ gridR Map.!? (x,y) == Just Empty && not (any ((== (Just $ Filled c)) . (gridR Map.!?)) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]) | x <- [0 .. width grid - 1], y <- [0 .. height grid -1], c <- Set.toList colours]
