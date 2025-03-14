-- This is the main entry point for your Gridlock application.

import Gridlock.DrawGrid ( drawGrid )
import Gridlock.Parser ( parseGame )
import Gridlock.Types ( GameRecord(grids, players) )
import System.Environment ( getArgs )
import Text.Megaparsec ( runParserT, errorBundlePretty )
import Data.Foldable ( traverse_ )
import Control.Monad.State.Lazy ( evalState )

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> traverse_ putStrLn ["Error: no file provided to parse.", "Please provide a file when running the program with", "  stack run --filename.gridlock"]
        f:_ -> do
            fContents <- readFile f
            putStrLn $ "Displaying " ++ f ++ ":"

            case evalState (runParserT parseGame "" fContents) [] of
                Left e -> putStrLn $ errorBundlePretty e
                Right record -> do
                    {-  The following will throw an error if the grids record list is empty. However, this cannot occur as
                        every GameRecord, even one for a 0x0 game, will start with an empty grid so there will always be
                        at least one item (the empty grid) in the grids record list. -}
                    let startingGrid = head $ grids record
                    let otherGrids = tail $ grids record

                    putStrLn $ drawGrid startingGrid
                    mapM_ (\(p,g) -> putStrLn (p ++ " plays:") >> putStrLn (drawGrid g)) (zip (cycle $ players record) otherGrids)

                    putStrLn $ (players record !! (length otherGrids `mod` 2)) ++ " can't move."
                    putStrLn $ (players record !! ((length otherGrids + 1) `mod` 2)) ++ " wins!"
