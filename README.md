# CS141 Coursework 2 2025: Gridlock

This repository contains my submission to the second coursework of the [CS141 Functional Programming module](https://courses.warwick.ac.uk/modules/2024/CS141-15) at the University of Warwick, from when I took the module in 2025/2025 (my first year). The module was lectured by [Dr. Alex Dixon](https://warwick.ac.uk/fac/sci/dcs/people/alex_dixon/).

I received a "high 1st", as defined by the [University's Undergraduate Mark Scale](https://warwick.ac.uk/services/aro/dar/quality/categories/examinations/marking/ug/), on my submission, which made use the [Megaparsec library](https://hackage.haskell.org/package/megaparsec), as well as monad transformers (which I taught to myself for the coursework).

## Coursework description

Gridlock is a two-player strategy game played on an N by M grid of cells, which is initially empty. On a player's turn, they must fill in an empty cell with a colour of their choice from a pre-determined list of allowed colours. The colour used must not be the colour of any adjacent cells. The last player able to make such a move wins.

Gridlock games can be described in `.gridlock` files, described below. The goal of the coursework was to parse `.gridlock` files and display the games they describe in the terminal. I went beyond the specification to also allow parsing of malformed `.gridlock` files as well, and also developed a basic command line interface using the [optparse-applicative library](https://hackage.haskell.org/package/optparse-applicative).

### `.gridlock` files

Some example Gridlock files can be found in the `examples/pass` folder. 

All `.gridlock` files must start with the following line:
```
GRIDLOCK
```
This is then followed by a list of players (exactly two, with the first player to move listed first), the grid size (as `NxM`, where `N` and `M` are both non-negative integers), and a list of colours used for the game (the set of available colours is red, green, yellow, blue, magenta, and cyan):
```
Players: player 1 name, player 2 name.
Grid: NxM.
Colours: ,-separated list of colours.
```
Following this is a list of moves, all of which must be legal:
```
[player] plays [colour] at [coordinate].
```
Finally, the file must end with
```
[player] cannot move.
[player] wins!
```
to indicate the losing and winning players.

## Documentation

Documentation, along with explanation of how I developed my code, is available in the file titled `report.pdf`.

## Note to future students

Please carefully consider the rules on use of other people's code and plagiarism when reading through this solution, and remember that copying work from any outside source is not considered appropriate academic conduct.
