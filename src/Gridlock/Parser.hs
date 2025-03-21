{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
module Gridlock.Parser where

import Text.Megaparsec ( MonadParsec(try, lookAhead), ParsecT, manyTill, choice, (<|>), many, some, optional, manyTill, eof, lookAhead )
import Text.Megaparsec.Char ( string, newline, alphaNumChar, char, space, space1, hspace )
import Text.Megaparsec.Char.Lexer ( decimal )
import Control.Monad.State.Lazy ( MonadState(put, get), modify, MonadTrans(lift), State )
import Control.Monad ( void, when, unless )
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void ( Void )
import Gridlock.Types
    ( GameRecord(..),
      Colour(..),
      Cell(Filled, Empty),
      Grid(Grid, rep, width, height),
      Player, displayColour)
import Gridlock.Utils (getPlayerInNMoves)

{- | A monad stack formed of @State@ wrapped in @ParsecT@. This allows for parsing while also
    threading a @GameRecord@ through, gradually referring to it and adding to it to produce
    the final result -}
type StatefulParser = ParsecT Void String (State GameRecord)

addGrid :: Grid -> State GameRecord ()
addGrid g = modify (\gr -> gr {grids = grids gr ++ [g]})

addPlayer :: Player -> State GameRecord ()
addPlayer p = modify (\gr -> gr {players = players gr ++ [p]})

addColour :: Colour -> State GameRecord ()
addColour c = modify (\gr -> gr {colours = Set.insert c $ colours gr})

{- | Parse a .gridlock file into a @GameRecord@. If the Boolean parameter is @False@, this will be
    done strictly, exactly following the syntax in the coursework spec. Otherwise, more freedom
    in the syntax and room for ambiguity (hence the parameter name) is allowed.
-}
parseGame :: Bool -> StatefulParser GameRecord
parseGame ambiguity = do
    let optionality = (if ambiguity then optional else fmap Just) :: StatefulParser a -> StatefulParser (Maybe a)
    let whitespace = (if ambiguity then space else void $ some newline) :: StatefulParser ()

    --  Parse the top of the file, extracing players, game size and colours.
    when ambiguity space -- Allow arbitrary whitespace at the start of the file, only in ambiguous mode.
    optionality $ string "GRIDLOCK"
    whitespace
    optionality $ parsePlayers ambiguity
    whitespace
    (width, height) <- parseSize ambiguity
    whitespace
    optionality $ parseColours ambiguity
    whitespace

    {-  @parseGameStrict@ should be evaluated with @emptyRecord@ as its initial state. This state
        is then modified to contain an empty @Grid@. -}
    lift $ addGrid Grid
            { width = width
            , height = height
            , rep = Map.fromList $ [((x,y), Empty) | x <- [0 .. width-1], y <- [0 .. height-1]]
            }

    {-  Parse lines describing moves until "cannot move" or "wins!" lines are reached, or the file ends. -}
    manyTill (parseMove ambiguity) (try $ choice [void $ lookAhead $ manyTill alphaNumChar $ string " cannot move", void $ lookAhead $ manyTill alphaNumChar $ string " wins!", lookAhead eof])

    {-  Deal with the possibility that the current stateful @GameRecord@ might not contain both players'
        names, filling it in with dummy names as required. -}
    modify (\gr ->
        let currentPlayers = players gr
        in gr {players = case length currentPlayers of
                0 -> ["Player 1", "Player 2"]
                1 -> [head currentPlayers, "Player 2"]
                _ -> currentPlayers
            })

    --  The stateful @GameRecord@ is now complete.
    finalRecord <- get

    {-  Parse the "cannot move", "wins!" lines that should be at the end of the file and check that
        the loss is valid. -}
    let winner = getPlayerInNMoves 0 finalRecord
    let loser = getPlayerInNMoves 1 finalRecord
    optionality $ string $ loser ++ " cannot move."
    canMove <- lift checkCanMove
    if canMove then
        fail $ "Invalid loss as " ++ loser ++ " can move."
    else do
        whitespace
        optionality $ string $ winner ++ " wins!"
        --fail $ show finalRecord
        pure finalRecord

{- | Parse a line of text describing a Gridlock move, verifying that it is a valid move. If the 
    Boolean parameter is @False@, this will be done strictly, exactly following the syntax in the
    coursework spec. Otherwise, more freedom in the syntax and room for ambiguity (hence the
    parameter name) is allowed.
-}
parseMove :: Bool -> StatefulParser ()
parseMove ambiguity = do
    -- Parse the name and validate that it is valid.
    get >>= \record -> do
        player <- manyTill alphaNumChar $ lookAhead $ string " plays "
        {-  Add the player to the stateful @GameRecord@ if there are less than two recorded players and the player is
            not already recorded. When parsing strictly, @length (players record) == 2@ already so this doesn't triger. -}
        when (length (players record) < 2) $ unless (player `elem` players record) $ lift $ addPlayer player


        -- Validate that the player's name is valid and that it is this player's turn.
        newRecord <- get
        unless (player `elem` players newRecord) $ fail $ player ++ " is not a valid player."
        unless (getPlayerInNMoves 1 newRecord == player) $ fail $ player ++ " is playing out of turn. It is " ++ getPlayerInNMoves 1 newRecord ++ "'s turn."
        -- Note: the second case should cover the first, but the first is included to produce a better error message.

    string " plays "

    -- Parse the move's colour, verifying that it is a valid colour if doing so strictly. 
    moveColour <- get >>= \record -> choice $ map parseColour (if ambiguity then [Red .. Cyan] else Set.toList $ colours record)
    {-  When parsing ambiguously, add the colour to the stateful @GameRecord@. As the colours are stored in a @Set@,
        this will do nothing if the colour is already present. -}
    when ambiguity $ lift $ addColour moveColour

    -- Parse the move's position and verify that the resulting move is valid.
    get >>= \record -> do
        let lastGrid = last $ grids record
        let lastGridRep = rep lastGrid

        string " at "
        (x,y) <- parseCoords

        -- Verify that the move is valid and error if it isn't.
        if x < 0 || x >= width lastGrid || y < 0 || y >= height lastGrid then
            fail $ "Move at " ++ show (x,y) ++ " is invalid as it is out of bounds."
        else if lastGridRep Map.! (x,y) /= Empty then
            fail $ "Move at " ++ show (x,y) ++ " is invalid as this cell is already filled."
        else if any ((== (Just $ Filled moveColour)) . (lastGridRep Map.!?)) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)] then
            -- ^ Test every surrounding cell to see if it has the colour of the current move.
            fail $ "Move at " ++ show (x,y) ++ " is invalid as an adjacent cell already has this colour."
        else do
            (if ambiguity then optional else fmap Just) $ char '.' -- Require a full stop when parsing strictly.

            -- Record the move in the stateful @GameRecord@.
            lift $ addGrid $ lastGrid {rep = Map.adjust (const $ Filled moveColour) (x,y) lastGridRep}
            (if ambiguity then space else void $ some newline) :: StatefulParser () -- allow arbitrary whitespace when parsing ambiguously, otherwise just newlines.
    where
        -- | Parse a set of coordinates written as "(x,y)".
        parseCoords :: StatefulParser (Int, Int)
        parseCoords = do
            char '('
            x <- decimal
            char ','
            y <- decimal
            char ')'
            pure (x,y)

{- | @parsePlayers@ parses the names of the players when they're declared at the top of a .gridlock file. 
    If the Boolean parameter is False, this will be done strictly, requiring two names to be specified as
    in the coursework spec. Otherwise, this is done non-strictly, allowing only a single name to be listed,
    and for arbitrary horizontal whitespace in between the two names if two names are listed. -}
parsePlayers :: Bool -> StatefulParser ()
parsePlayers ambiguity = do
    string "Players:"
    if ambiguity then
        choice
            [ try $ do
                hspace
                p1 <- manyTill alphaNumChar $ char ','
                hspace
                p2 <- manyTill alphaNumChar (space1 <|> void (char '.'))
                lift $ addPlayer p1
                lift $ addPlayer p2
            , do
                hspace
                p <- manyTill alphaNumChar (space1 <|> void (char '.'))
                lift $ addPlayer p
            ]
    else do
        char ' '
        p1 <- manyTill alphaNumChar $ char ','
        char ' '
        p2 <- manyTill alphaNumChar $ char '.'
        lift $ addPlayer p1
        lift $ addPlayer p2

{- | @parseSize@ parses the size of the grid when it is defined at the top of a .gridlock file. 
    If the Boolean parameter is False, this will be done strictly, requiring the exact spacing
    and full stop set out in the coursework spec. Otherwise, leeway is allowed here. -}
parseSize :: Bool -> StatefulParser (Int, Int)
parseSize ambiguity = do
    string "Grid:"
    if ambiguity then hspace else void $ char ' '
    w <- decimal
    when ambiguity hspace
    char 'x'
    when ambiguity hspace
    h <- decimal
    (if ambiguity then optional else fmap Just) $ char '.'
    pure (w, h)

{- | @parseColours@ parses the colours defined at the top of a .gridlock file. If the Boolean parameter
    is False, this will be done strictly, requiring the exact spacing and full stop set out in the
    coursework spec. Otherwise, leeway is allowed here. -}
parseColours :: Bool -> StatefulParser ()
parseColours ambiguity = do
    string "Colours:"
    if ambiguity then hspace else void $ char ' '
    -- Handle the first colour separately as it doesn't have a proceeding comma.
    c1 <- colour
    colours <- many $ do
        char ','
        if ambiguity then hspace else void $ char ' '
        colour
    (if ambiguity then optional else fmap Just) $ char '.'

    let coloursSet = Set.fromList $ c1 : colours
    oldRecord <- get
    put oldRecord { colours = coloursSet }

    where
        -- | Matches to any @Colour@'s lower-case string representation, returning that @Colour@.
        colour :: StatefulParser Colour
        colour = choice $ map parseColour [Red .. Cyan]

{- | Used in @parseMove@ and @parseColours@, @parseColour@ takes a colour, matches to its lower-case
    @String@ representation and returns the colour. -}
parseColour :: Colour -> StatefulParser Colour
parseColour c = string (displayColour c) >> pure c

{- | @checkCanMove@ looks at the @GameRecord@ currently in the state and determines whether any moves can be made from this position.
    To do this, it checks each @Cell@ and each @Colour@, checking that the @Cell@ is empty and that there's a @Colour@ in the game not
    currently used to fill any adjacent @Cell@. If both of these conditions are met for any cell, then there's still a move that can
    be made and True is returned. -}
checkCanMove :: State GameRecord Bool
checkCanMove = do
    record <- get
    let cs = colours record
    let g = last $ grids record
    let gR = rep g
    pure $ or [
            gR Map.!? (x,y) == Just Empty && not ( -- check that each cell is empty and that ...
                any ((== (Just $ Filled c)) . (gR Map.!?)) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)] -- ... none of the adjacent cells are filled with any colour
                {-  Note: @(Map.!?)@ returns a @Maybe@ value. This means @Nothing@ is returned if the cell checked doesn't exist, accounting
                    for cells on the edge of the grid that don't have the full four adjacent cells. As this will never be equal to a @Just@
                    value, this case will be effectively handled. -}
            ) | x <- [0 .. width g - 1], y <- [0 .. height g -1], c <- Set.toList cs -- for each cell (with coords (x,y)) and colour
        ]
