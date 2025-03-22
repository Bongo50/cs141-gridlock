module Gridlock.DrawGrid where

import Gridlock.ColourSquares ( square )
import Gridlock.Types ( Cell(..), Grid(..) ) 
import Data.Map qualified as Map

{- | @drawGrid@ takes a @Grid@ and outputs it as a human-readable @String@. It primarily works by using list comprehension
    to iterate through every key in the @Grid@'s @rep@, a @Map Coord Cell@. A helper function, @drawCell@, is then used to
    obtain the @String@ used to display the cell in the final output. These are concatonated with suitable borders. -}
drawGrid :: Grid -> String
drawGrid Grid{..} = 
    -- Record wildcards has been used here to expose all of the input @Grid@'s fields.
    hBorder ++ "\n" ++
    concatMap addVBorder [ concatMap (drawCell . (rep Map.!)) [(x,y) | x <- [0 ..  width - 1]] | y <- [0 .. height - 1] ]
    {-  Note on the use of the partial function @(Map.!)@: this will error if the provided key doesn't exist in 
        the provided @Map@, @rep g@. However, as long as the provided @Grid@, @g@, contains the correct width and
        height, and has a complete @rep@ @Map@ (without any missing keys), this cannot happen as only keys that will
        be in the map will be accessed. The non-partial alternative @(Map.!?)@ adds in the extra step of outputing
        a @Maybe Cell@ which would then require extracting the value in the case of a @Just@, or handling a @Nothing@
        case. -}
    ++ hBorder
    where
        -- | Horizontal border
        hBorder :: String
        hBorder = "+" ++ replicate width '-' ++ "+"

        -- | @addVBorder@ is a function that adds a vertical border to both sides of the input @String@.
        addVBorder :: String -> String
        addVBorder l = "|" ++ l ++ "|\n"

        {- | @drawCell@ is a function that takes a @Cell@ and returns the @String@ representation. @Empty@ @Cell@s are the
            @String@ " ", while @Filled@ @Cell@s use the @square@ function from the @Gridlock.ColourSquares@ module. -}
        drawCell :: Cell -> String
        drawCell Empty = " "
        drawCell (Filled col) = square col
