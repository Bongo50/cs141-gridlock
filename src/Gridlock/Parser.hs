{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Gridlock.Parser where

import Text.Megaparsec ( MonadParsec(try, lookAhead), ParsecT, manyTill, choice, (<|>), many, some, optional, manyTill, eof, lookAhead )
import Text.Megaparsec.Char ( string, newline, alphaNumChar, char, space, space1, hspace )
import Text.Megaparsec.Char.Lexer ( decimal )
import Control.Monad ( void, when, unless, forM_ )
import Control.Monad.State.Lazy ( MonadState(get), modify, State )
import Control.Monad.Reader ( ReaderT, MonadReader(ask) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Void ( Void )
import Gridlock.Types
    ( GameRecord(..),
      Colour(Cyan, Red),
      Cell(Filled, Empty),
      Grid(..),
      Player )
import Gridlock.Utils (getPlayerInNMoves, displayColour)

{- | A monad stack formed of @State@ wrapped in @ParsecT@, wrapped in @ReaderT@. This allows
    for parsing (via @ParsecT@) while also threading a @GameRecord@ through, gradually referring to it and
    adding to it to produce the final result (via @State@), and also allowing a Boolean value indicating
    whether ambiguous parsing is allowed to be read (via @ReaderT@). -}
type MonadStack = ReaderT Bool (ParsecT Void String (State GameRecord))

--------------------------------------------------------------------------------
-- Utilities:
-- The types of these functions allow them to be used in @MonadStack@ functions without the use of @lift@s.

-- The following functions are used in various places to add to each field of the stateful @GameRecord@ component.
-- | Adds a @Grid@ to the stateful @GameRecord@.
addGrid :: MonadState GameRecord m => Grid -> m ()
addGrid g = modify (\gr -> gr {grids = grids gr ++ [g]})

-- | Adds a @Player@ to the stateful @GameRecord@.
addPlayer :: MonadState GameRecord m => Player -> m ()
addPlayer p = modify (\gr -> gr {players = players gr ++ [p]})

-- | Adds a @Colour@ to the stateful @GameRecord@.
addColour :: MonadState GameRecord m => Colour -> m ()
addColour c = modify (\gr -> gr {colours = Set.insert c $ colours gr})


{- | @checkCanMove@ looks at the @GameRecord@ currently in the state and determines whether any moves can be made from this position.
    To do this, it checks each @Cell@ and each @Colour@, checking that the @Cell@ is empty and that there's a @Colour@ in the game not
    currently used to fill any adjacent @Cell@. If both of these conditions are met for any cell, then there's still a move that can
    be made and True is returned. -}
checkCanMove :: MonadState GameRecord m => m Bool
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


--------------------------------------------------------------------------------
-- Parser:

-- | Parse a .gridlock file into a @GameRecord@.
parseGame :: MonadStack GameRecord
parseGame = do
    {- If the Boolean value below is @False@, parsing will be done strictly, exactly following the syntax
    in the coursework spec. Otherwise, more freedom in the syntax and room for ambiguity (hence the
    parameter name) is allowed. -}
    allowAmbiguity <- ask
    -- If ambiguity is allowed, some things are optional.
    let optionality = (if allowAmbiguity then optional else fmap Just) :: MonadStack a -> MonadStack (Maybe a)
    -- If ambiguity is allowed, more relaxed whitespace is allowed.
    let whitespace = (if allowAmbiguity then space else void $ some newline) :: MonadStack ()

    --  Parse the top of the file, extracing players, game size and colours.
    when allowAmbiguity space -- Allow arbitrary whitespace at the start of the file, only in ambiguous mode.
    optionality $ string "GRIDLOCK"
    whitespace
    optionality parsePlayers
    whitespace
    (width, height) <- parseSize
    whitespace
    optionality parseColours
    whitespace

    {-  @parseGameStrict@ should be evaluated with @emptyRecord@ as its initial state. This state
        is then modified to contain an empty @Grid@. -}
    addGrid Grid
        { width = width
        , height = height
        , rep = Map.fromList $ [((x,y), Empty) | x <- [0 .. width-1], y <- [0 .. height-1]]
        }

    {-  Parse lines describing moves until "cannot move" or "wins!" lines are reached, or the file ends. -}
    manyTill parseMove (try $ choice [void $ try $ lookAhead $ manyTill alphaNumChar $ string " cannot move", void $ try $ lookAhead $ manyTill alphaNumChar $ string " wins!", lookAhead eof])

    {-  Deal with the possibility that the current stateful @GameRecord@ might not contain both players'
        names, filling it in with dummy names as required. -}
    modify (\gr@GameRecord{..} ->
        gr {players = case length players of
                0 -> ["Player 1", "Player 2"]
                1 -> [head players, "Player 2"]
                _ -> players
            })

    --  The stateful @GameRecord@ is now complete.
    finalRecord <- get

    {-  Parse the "cannot move", "wins!" lines that should be at the end of the file and check that
        the loss is valid. -}
    let winner = getPlayerInNMoves 0 finalRecord -- The winner is the player who played last.
    let loser = getPlayerInNMoves 1 finalRecord -- The loser would be the person playing next, but they can't.
    -- When parsing strictly, only allow the first option below. Otherwise, allow any.
    (if allowAmbiguity then choice else head)
        [ try (string (loser ++ " cannot move.") >> whitespace >> string (winner ++ " wins!") >> eof)
        , try (string (loser ++ " cannot move.") >> eof)
        , try (string (winner ++ " wins!") >> eof)
        , eof ]
    canMove <- checkCanMove
    if canMove then
        fail $ "Invalid loss as " ++ loser ++ " can move."
    else do
        pure finalRecord

-- | @parsePlayers@ parses the names of the players when they're declared at the top of a .gridlock file.
parsePlayers :: MonadStack ()
parsePlayers = do
    {- If the Boolean value below is @False@, parsing will be done strictly, requiring two names to be
    specified as in the coursework spec. Otherwise, this is done non-strictly, allowing only a single
    name to be listed, and for arbitrary horizontal whitespace in between the two names if two names
    are listed. -}
    allowAmbiguity <- ask

    string "Players:"
    if allowAmbiguity then
        choice
            [ try $ do
                hspace
                p1 <- manyTill alphaNumChar $ char ','
                hspace
                p2 <- manyTill alphaNumChar (space1 <|> void (char '.'))
                addPlayer p1
                addPlayer p2
            , do
                hspace
                p <- manyTill alphaNumChar (space1 <|> void (char '.'))
                addPlayer p
            ]
    else do
        char ' '
        p1 <- manyTill alphaNumChar $ char ','
        char ' '
        p2 <- manyTill alphaNumChar $ char '.'
        addPlayer p1
        addPlayer p2

-- | @parseSize@ parses the size of the grid when it is defined at the top of a .gridlock file.
parseSize :: MonadStack (Int, Int)
parseSize = do
    {- If the Boolean value below is @False@, parsing will be done strictly, requiring the exact spacing
    and full stop set out in the coursework spec. Otherwise, leeway is allowed here. -}
    allowAmbiguity <- ask

    string "Grid:"
    if allowAmbiguity then hspace else void $ char ' '
    w <- decimal
    when allowAmbiguity hspace
    char 'x'
    when allowAmbiguity hspace
    h <- decimal
    (if allowAmbiguity then optional else fmap Just) $ char '.'
    pure (w, h)

-- | @parseColours@ parses the colours defined at the top of a .gridlock file.
parseColours :: MonadStack ()
parseColours = do
    {- If the Boolean value below is @False@, parsing will be done strictly, requiring the exact spacing
    and full stop set out in the coursework spec. Otherwise, leeway is allowed here. -}
    allowAmbiguity <- ask

    string "Colours:"
    if allowAmbiguity then hspace else void $ char ' '
    -- Handle the first colour separately as it doesn't have a proceeding comma.
    c1 <- colour
    colours <- many $ do
        char ','
        if allowAmbiguity then hspace else void $ char ' '
        colour
    (if allowAmbiguity then optional else fmap Just) $ char '.'

    forM_ (c1 : colours) addColour

    where
        -- | Matches to any @Colour@'s lower-case string representation, returning that @Colour@.
        colour :: MonadStack Colour
        colour = choice $ map parseColour [Red .. Cyan]

-- | Parse a line of text describing a Gridlock move, verifying that it is a valid move. 
parseMove :: MonadStack ()
parseMove = do
    {- If the Boolean value below is @False@, parsing will be done strictly, exactly following the syntax
    in the coursework spec. Otherwise, more freedom in the syntax and room for ambiguity (hence the
    parameter name) is allowed. -}
    allowAmbiguity <- ask

    -- Parse the name and validate that it is valid.
    (get >>= \GameRecord{players} -> do
        player <- manyTill alphaNumChar $ lookAhead $ string " plays "
        {-  Add the player to the stateful @GameRecord@ if there are less than two recorded players and the player is
            not already recorded. When parsing strictly, @length (players record) == 2@ already so this doesn't triger. -}
        when (length players < 2) $ unless (player `elem` players) $ addPlayer player
        pure player) >>= \player ->
            
        -- Validate that the player's name is valid and that it is this player's turn.
        get >>= \record@GameRecord{players} -> do
            unless (player `elem` players) $ fail $ player ++ " is not a valid player."
            unless (getPlayerInNMoves 1 record == player) $ fail $ player ++ " is playing out of turn. It is " ++ getPlayerInNMoves 1 record ++ "'s turn."
            -- Note: the second case should cover the first, but the first is included to produce a better error message.
    -- The unconventional syntax above is so that record punning can be used on the results of both @get@s, and so @player@ can be used in both blocks.

    string " plays "

    -- Parse the move's colour, verifying that it is a valid colour if doing so strictly. 
    moveColour <- get >>= \GameRecord{colours} -> choice $ map parseColour (if allowAmbiguity then [Red .. Cyan] else Set.toList colours)
    {-  When parsing ambiguously, add the colour to the stateful @GameRecord@. As the colours are stored in a @Set@,
        this will do nothing if the colour is already present. -}
    when allowAmbiguity $ addColour moveColour

    -- Parse the move's position and verify that the resulting move is valid.
    get >>= \GameRecord{grids} -> do
        -- Get the field's from the last grid with record wildcards.
        let lastGrid@Grid{..} = last grids

        string " at "
        (x,y) <- parseCoords

        -- Verify that the move is valid and error if it isn't.
        if x < 0 || x >= width || y < 0 || y >= height then
            fail $ "Move at " ++ show (x,y) ++ " is invalid as it is out of bounds."
        else if rep Map.! (x,y) /= Empty then
            fail $ "Move at " ++ show (x,y) ++ " is invalid as this cell is already filled."
        else if any ((== (Just $ Filled moveColour)) . (rep Map.!?)) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)] then
            -- ^ Test every surrounding cell to see if it has the colour of the current move.
            fail $ "Move at " ++ show (x,y) ++ " is invalid as an adjacent cell already has this colour."
        else do
            (if allowAmbiguity then optional else fmap Just) $ char '.' -- Require a full stop when parsing strictly.

            -- Record the move in the stateful @GameRecord@.
            addGrid $ lastGrid {rep = Map.adjust (const $ Filled moveColour) (x,y) rep}
            (if allowAmbiguity then space else void $ some newline) :: MonadStack () -- allow arbitrary whitespace when parsing ambiguously, otherwise just newlines.
    where
        -- | Parse a set of coordinates written as "(x,y)".
        parseCoords :: MonadStack (Int, Int)
        parseCoords = do
            char '('
            x <- decimal
            char ','
            y <- decimal
            char ')'
            pure (x,y)

{- | Used in @parseMove@ and @parseColours@, @parseColour@ takes a colour, matches to its lower-case
    @String@ representation and returns the colour. -}
parseColour :: Colour -> MonadStack Colour
parseColour c = string (displayColour c) >> pure c