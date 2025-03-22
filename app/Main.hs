-- Main entry point.
import Gridlock.DrawGrid ( drawGrid )
import Gridlock.Parser ( parseGame )
import Gridlock.Types ( GameRecord(grids, players) )
import Gridlock.Utils ( fileExtension, emptyRecord, getPlayerInNMoves )
import Gridlock.Args ( programArgs, RuntimeArgs(ambiguous, filename) )
import Text.Megaparsec ( runParserT, errorBundlePretty )
import Control.Monad.State.Lazy ( evalState )
import Control.Monad.Reader ( runReaderT )
import Options.Applicative ( execParser )

{- | The entrypoint to the program. This function handles arguments, runs the parser and outputs the game in a human-
    readable way. -}
main :: IO ()
main = do
    -- Parse and handle arguments using optparse-applicative.
    args <- execParser programArgs
    let file = filename args

    -- Check the file extension.
    if not (ambiguous args) && (drop (length file - length fileExtension) file /= fileExtension) then do
        putStrLn $ "Can't read " ++ file ++ " as it is not a .gridlock file."
        putStrLn "Consider using the ambiguous -a flag."
    else do
        fContents <- readFile file
        putStrLn $ "Displaying " ++ file ++ ":"

        -- Run the parser.
        case evalState (runParserT (runReaderT parseGame $ ambiguous args) file fContents) emptyRecord of
            Left e -> putStrLn $ errorBundlePretty e
            Right record -> do
                {-  The following will throw an error if the @grids record@ list is empty. However, this cannot occur as
                    every @GameRecord@, even one for a 0x0 game, will start with an empty @Grid@ so there will always be
                    at least one item (the empty @Grid@) in the @grids record@ list. -}
                let startingGrid = head $ grids record
                let otherGrids = tail $ grids record

                -- The starting grid is drawn separetely as no-one plays to produce it.
                putStrLn $ drawGrid startingGrid
                -- Output each grid, along with who played the move to produce it.
                mapM_ (\(p,g) -> putStrLn "" >> putStrLn (p ++ " plays:") >> putStrLn (drawGrid g)) (zip (cycle $ players record) otherGrids)

                -- Output informtion about the winner and looser. 
                putStrLn ""
                putStrLn $ getPlayerInNMoves 1 record ++ " can't move."
                putStrLn $ getPlayerInNMoves 0 record ++ " wins!"
