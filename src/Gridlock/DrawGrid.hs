module Gridlock.DrawGrid where

import Gridlock.ColourSquares (square)
import Gridlock.Types ( Cell(..), NxMGrid(rep'sq) )

import Data.Map qualified as Map

{-  drawGrid takes a Grid and outputs it as a human-readable String. It primarily works by using list comprehension
    to iterate through every key in the Grid's rep Map Coord Cell. A helper function, drawCell, is then used to
    obtain the String used to display the cell in the final output. These are concatonated with suitable borders. -}
drawGrid :: NxMGrid @n @m -> String
drawGrid g = hBorder ++ "\n" ++
    {-  Note on the use of the partial function (Map.!): this will error if the provided key doesn't exist in 
        the provided Map, r = repG. However, as long as the provided Grid, g, contains the correct width and
        height, and has a complete rep Map (without any missing keys), this cannot happen as only keys that will
        be in the map will be accessed. The non-partial alternative (Map.!?) adds in the extra step of outputing
        a Maybe Cell which would then require extracting the value in the case of a Just, or handling a Nothing
        case. -}
    concatMap addVBorder [ concatMap (drawCell . (rep g Map.!)) [(x,y) | x <- [0 ..  width g - 1]] | y <- [0 .. height g - 1] ]
    ++ hBorder
    where
        width = natVal $ Proxy @n :: Integer
        height = natVal $ Proxy @m :: Integer

        --  Horizontal border
        hBorder :: String
        hBorder = "+" ++ replicate (width g) '-' ++ "+"

        {-  addVBorder is a function that adds a vertical border to both sides of the input String. In this
            context, it will be a line of the output, hence l. -}
        addVBorder :: String -> String
        addVBorder l = "|" ++ l ++ "|\n"

        {-  drawCell is a function that takes a Cell and returns the string representation. Empty cells are the
            string " ", while Filled cells use the square function from the Gridlock.ColourSquares module. -}
        drawCell :: Cell -> String
        drawCell Empty = " "
        drawCell (Filled col) = square col
