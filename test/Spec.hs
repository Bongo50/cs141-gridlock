{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Tasty is the testing library that is used to specify tests.
The backends "tasty-hunit" and "tasty-quickcheck" specify the way that unit
tests and property tests (respectively) are written.
-}
{-# OPTIONS_GHC -Wno-all #-}

import Data.Function (on)
import Data.List ( findIndex, inits, isInfixOf, nub, isSuffixOf, sort )
import Data.Tuple (swap)
import System.Console.ANSI as ANSI hiding (Color (..))
import qualified System.Console.ANSI as ANSI
import Test.Tasty (
    TestTree,
    testGroup,
 )
import Test.Tasty.HUnit
import Test.Tasty.Muffled (muffledMain)
import Test.Tasty.QuickCheck

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Char8 (pack)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath (dropExtension, takeExtension)

import Gridlock.DrawGrid
import Gridlock.Parser
import Gridlock.Types

import Control.Monad (forM)
import System.Directory.Internal.Prelude (getArgs)
import Text.Megaparsec
import Control.Monad.State.Lazy ( evalState )
import Control.Monad.Reader ( runReaderT )

main :: IO ()
main = do
    x <- getArgs
    case x of
        ("generateExpected" : _) -> genExpected
        _ -> runTests

{- | Generate the expected outputs for the tests in the "pass" folder.
  DO NOT USE THIS unless you have changed GameRecord in a meaningful way and you are confident that your parser is producing correct game records.

  You can run it with stack test --test-arguments="generateExpected"
-}
genExpected = do
    putStrLn "Generating expected outputs..."
    passingFiles <-
        filter (\fp -> takeExtension fp == ".gridlock")
            <$> getDirectoryContents "examples/parsing/pass"
    passingCases <- forM (sort passingFiles) $ \fp -> do
        let fullPath = "examples/parsing/pass/" ++ fp
        let expectedPath = dropExtension fullPath <> ".expected"
        input <- readFile fullPath
        case evalState (runParserT (runReaderT parseGame False) fp input) emptyRecord of
            Left err -> do
                putStrLn $ "Parse error: " ++ errorBundlePretty err
            Right res -> do
                writeFile expectedPath $ show res
    putStrLn "Done."

runTests = do
    passingFiles <-
        filter (\fp -> takeExtension fp == ".gridlock")
            <$> getDirectoryContents "examples/parsing/pass"
    passingCases <- forM (sort passingFiles) $ \fp -> do
        let fullPath = "examples/parsing/pass/" ++ fp
            expectedPath = dropExtension fullPath <> ".expected"
        hasExpected <- doesFileExist expectedPath
        let testName = if hasExpected then fp else fp ++ " (no expected output)"
        let test = testCase testName do
                input <- readFile fullPath
                if hasExpected
                    then do
                        shouldBe <- read <$> readFile expectedPath
                        case evalState (runParserT (runReaderT parseGame False) fp input) emptyRecord of
                            Left err -> assertFailure $ "Parse error: " ++ errorBundlePretty err
                            Right res -> res @?= shouldBe
                    else case evalState (runParserT (runReaderT parseGame False) fp input) emptyRecord of
                        Left err -> assertFailure $ "Parse error: " ++ errorBundlePretty err
                        Right _ -> pure () -- Assume all good
        return test
    failingFiles <-
        filter (".gridlock" `isSuffixOf`)
            <$> getDirectoryContents "examples/parsing/fail"
    failingCases <- forM (sort failingFiles) $ \fp -> do
        let fullPath = "examples/parsing/fail/" ++ fp
        let test = testCase fp do
                input <- readFile fullPath
                case evalState (runParserT (runReaderT parseGame False) fp input) emptyRecord of
                    Left _ -> return ()
                    Right res ->
                        assertFailure $
                            "Expected parse error, but parsing was successful.\n"
                                <> "Result was the following game:\n"
                                <> setSGRCode [Reset]
                                <> show res
        return test

    let
        parserTests =
            testGroup
                "Parser tests"
                [ testGroup "Passing cases (should all parse correctly)" passingCases
                , testGroup "Failing cases (should all fail to parse)" failingCases
                ]

    clearScreen
    muffledMain $ testGroup "Tests" [drawGridTests, parserTests]

drawGridTests :: TestTree
drawGridTests =
    testGroup
        "drawGrid tests"
        [ testCase "The empty grid renders correctly" $
            let
                grid = Grid 0 0 Map.empty
                res = "++\n++"
             in
                gridCase grid res
        , testCase "A 1x1 with a single cell filled" $
            let
                grid = Grid 1 1 $ Map.fromList [((0, 0), Filled Red)]
                res = "+-+\n|\ESC[41mR\ESC[0m|\n+-+"
             in
                gridCase grid res
        , testCase "The example from the specification" $
            let
                grid =
                    Grid 3 3 $
                        Map.fromList
                            [ ((0, 0), Filled Red)
                            , ((1, 0), Empty)
                            , ((2, 0), Filled Red)
                            , ((0, 1), Empty)
                            , ((1, 1), Filled Green)
                            , ((2, 1), Empty)
                            , ((0, 2), Filled Red)
                            , ((1, 2), Empty)
                            , ((2, 2), Filled Red)
                            ]
                res = "+---+\n|\ESC[41mR\ESC[0m \ESC[41mR\ESC[0m|\n| \ESC[42mG\ESC[0m |\n|\ESC[41mR\ESC[0m \ESC[41mR\ESC[0m|\n+---+"
             in
                gridCase grid res
        , testCase "One row with all the colours" $
            let
                grid = Grid 6 1 $ Map.fromList $ Prelude.zip [(x, 0) | x <- [0 .. 5]] (map Filled [minBound .. maxBound])
                res = "+------+\n|\ESC[41mR\ESC[0m\ESC[42mG\ESC[0m\ESC[43mY\ESC[0m\ESC[44mB\ESC[0m\ESC[45mM\ESC[0m\ESC[46mC\ESC[0m|\n+------+"
             in
                gridCase grid res
        ]
  where
    gridCase grid res =
        if drawGrid grid == res
            then pure ()
            else
                assertFailure $
                    concat
                        [ "Tried to draw the following grid:\n"
                        , setSGRCode [Reset]
                        , show grid
                        , setSGRCode [SetColor Foreground Dull ANSI.Red]
                        , "\n  Expected to see this:\n"
                        , setSGRCode [Reset]
                        , res
                        , setSGRCode [SetColor Foreground Dull ANSI.Red]
                        , "\n  but got this:\n"
                        , setSGRCode [Reset]
                        , drawGrid grid
                        ]
