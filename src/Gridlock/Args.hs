module Gridlock.Args where
--  https://hackage.haskell.org/package/optparse-applicative-0.18.1.0#readme

import Options.Applicative
    ( (<**>),
      fullDesc,
      header,
      help,
      info,
      long,
      metavar,
      progDesc,
      short,
      strOption,
      switch,
      helper,
      ParserInfo,
      Parser )

-- | A data structure to store the arguments provided to the program at runtime via the terminal.
data RuntimeArgs = RuntimeArgs
    { filename      :: String
    , ambiguous     :: Bool }

-- | A description of the arguments that this program takes.
args :: Parser RuntimeArgs
args = RuntimeArgs
      <$> strOption
          ( long "file"
         <> long "filename"
         <> short 'f'
         <> metavar "<FILENAME>"
         <> help "Filename to read from" )
      <*> switch
          ( long "ambiguous"
         <> short 'a'
         <> help "Whether to allow less strict, potentially ambiguous, parsing" )

-- | A description of the program, including its arguments. The "--help" option is added here.
programArgs :: ParserInfo RuntimeArgs
programArgs = info (args <**> helper)
  ( fullDesc
  <> progDesc "Parse and display Gridlock games from .gridlock files"
  <> header "Gridlock Helper" )