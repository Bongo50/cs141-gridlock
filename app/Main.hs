-- This is the main entry point for your Gridlock application.

import Gridlock.DrawGrid ( drawGrid )
import Gridlock.Parser ( parseGame )
import Gridlock.Types ( GameRecord(grids, players), emptyRecord )
import Text.Megaparsec ( runParserT, errorBundlePretty )
import Control.Monad.State.Lazy ( evalState )

import Options.Applicative hiding (Parser)
import qualified Options.Applicative as Opt

fileExtension :: String
fileExtension = ".gridlock"

-- https://hackage.haskell.org/package/optparse-applicative-0.18.1.0#readme
data RuntimeArgs = RuntimeArgs 
    { filename      :: String 
    , ambiguous     :: Bool }

handleArgs :: Opt.Parser RuntimeArgs
handleArgs = RuntimeArgs
      <$> strOption
          ( long "filename"
         <> long "file"
         <> short 'f'
         <> metavar "FILENAME"
         <> help "Filename to read to/save from" )
      <*> switch
          ( long "ambiguous"
         <> short 'a'
         <> help "Whether to allow less strict, potentially ambiguous, parsing" )

opts :: ParserInfo RuntimeArgs
opts = info (handleArgs <**> helper)
  ( fullDesc
  <> progDesc "Parse and display Gridlock games from .gridlock files"
  <> header "Gridlock Helper" )

main :: IO ()
main = do
    args <- execParser opts
    let file = filename args

    if not (ambiguous args) && (drop (length file - length fileExtension) file /= fileExtension) then do
        putStrLn $ "Can't read " ++ file ++ " as it is not a .gridlock file."
        putStrLn "Consider using the ambiguous -a flag."
    else do
        fContents <- readFile file
        putStrLn $ "Displaying " ++ file ++ ":"

        case evalState (runParserT (parseGame $ ambiguous args) file fContents) emptyRecord of
            Left e -> putStrLn $ errorBundlePretty e
            Right record -> do
                {-  The following will throw an error if the @grids record@ list is empty. However, this cannot occur as
                    every @GameRecord@, even one for a 0x0 game, will start with an empty @Grid@ so there will always be
                    at least one item (the empty @Grid@) in the @grids record@ list. -}
                let startingGrid = head $ grids record
                let otherGrids = tail $ grids record

                putStrLn $ drawGrid startingGrid
                mapM_ (\(p,g) -> putStrLn "" >> putStrLn (p ++ " plays:") >> putStrLn (drawGrid g)) (zip (cycle $ players record) otherGrids)

                putStrLn ""
                putStrLn $ (players record !! ((length (grids record) + 1) `mod` 2)) ++ " can't move."
                putStrLn $ (players record !! (length (grids record) `mod` 2)) ++ " wins!"
