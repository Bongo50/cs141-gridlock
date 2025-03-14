-- This is the main entry point for your Gridlock application.

import Gridlock.DrawGrid ( drawGrid )
import Gridlock.Parser ( parseGame )
import Gridlock.Types ( GameRecord(grids, players) )
import System.Environment ( getArgs )
import Text.Megaparsec ( runParserT, errorBundlePretty )
import Data.Foldable ( traverse_ )
import Control.Monad.State.Lazy ( evalState )

import Options.Applicative hiding (Parser)
import qualified Options.Applicative as Opt

-- https://hackage.haskell.org/package/optparse-applicative-0.18.1.0#readme
data RuntimeArgs = RuntimeArgs 
    { filename      :: String 
    , strict        :: Bool }

handleArgs :: Opt.Parser RuntimeArgs
handleArgs = RuntimeArgs
      <$> strOption
          ( long "filename"
         <> long "file"
         <> short 'f'
         <> metavar "FILENAME"
         <> help "Filename to read to/save from" )
      <*> switch
          ( long "strict"
         <> short 's'
         <> help "Whether to parse strictly" )

opts :: ParserInfo RuntimeArgs
opts = info (handleArgs <**> helper)
  ( fullDesc
  <> progDesc "Parse and display Gridlock games from .gridlock files"
  <> header "Gridlock Helper" )


main :: IO ()
main = do
    args <- execParser opts

    fContents <- readFile $ filename args 
    putStrLn $ "Displaying " ++ filename args ++ ":"

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
