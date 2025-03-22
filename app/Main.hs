-- Main entry point.
import Gridlock.DrawGrid ( drawGrid )
import Gridlock.Parser ( parseGame )
import Gridlock.Types ( GameRecord(GameRecord, grids, players) )
import Gridlock.Utils ( fileExtension, emptyRecord, getPlayerInNMoves )
import Gridlock.Args ( programArgs, RuntimeArgs(RuntimeArgs, filename, ambiguous) ) 
import Text.Megaparsec ( runParserT, errorBundlePretty )
import Control.Monad.State.Lazy ( evalState )
import Control.Monad.Reader ( runReaderT )
import Options.Applicative ( execParser )

{- | The entry point to the program. This function handles arguments, runs the parser and outputs the game in a human-
    readable way. -}
main :: IO ()
main = do
    -- Parse and handle arguments using optparse-applicative.
    -- Record wildcards has been used here to expose all of the @RuntimeArgs@'s fields.
    RuntimeArgs{..} <- execParser programArgs

    -- Check the file extension.
    if not ambiguous && (drop (length filename - length fileExtension) filename /= fileExtension) then do
        {-  I considered using @takeExtension@ from System.FilePath (https://hackage.haskell.org/package/filepath-1.5.4.0/docs/System-FilePath.html)
            here, but it didn't seem worth importing the entire filepath library just for this one function. -}
        putStrLn $ "Can't read " ++ filename ++ " as it is not a .gridlock file."
        putStrLn "Consider using the ambiguous -a flag."
    else do
        fContents <- readFile filename
        putStrLn $ "Displaying " ++ filename ++ ":"

        -- Run the parser.
        case evalState (runParserT (runReaderT parseGame ambiguous) filename fContents) emptyRecord of
            Left e -> putStrLn $ errorBundlePretty e
            Right record@GameRecord{players, grids} -> do
                {-  The following will throw an error if the @grids record@ list is empty. However, this cannot occur as
                    every @GameRecord@, even one for a 0x0 game, will start with an empty @Grid@ so there will always be
                    at least one item (the empty @Grid@) in the @grids record@ list. -}
                let startingGrid = head grids
                let otherGrids = tail grids

                -- The starting grid is drawn separetely as no-one plays to produce it.
                putStrLn $ drawGrid startingGrid
                -- Output each grid, along with who played the move to produce it.
                mapM_ (\(p,g) -> putStrLn "" >> putStrLn (p ++ " plays:") >> putStrLn (drawGrid g)) (zip (cycle players) otherGrids)

                -- Output informtion about the winner and looser. 
                putStrLn ""
                putStrLn $ getPlayerInNMoves 1 record ++ " can't move."
                putStrLn $ getPlayerInNMoves 0 record ++ " wins!"
