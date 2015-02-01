module Main where

import Diamond

import Control.Monad
import Data.Char (chr, ord)
import Test.Hspec
import Test.QuickCheck

instance Arbitrary Element where
    arbitrary = elements [A .. Z]

nonEmpty = filter (/= None)

chop :: [a] -> [a]
chop [x] = []
chop xs = (tail . init) xs

transpose :: [[a]] -> [[a]]
transpose rows
    | all null rows = []
    | otherwise     = map head rows : transpose (map tail rows)

main = do
    mapM_ quickCheck
        [(label "The diamond has 2*N-1 rows, where N is the element"
             (\element -> (length $ diamond element) == fromEnum element * 2 - 1)),
         (label "The diamond has 2*N-1 columns, where N is the element"
             (\element -> all (\row -> length row == fromEnum element * 2 - 1) (diamond element))),
         (label "The diamond's rows go from A to N, where N is the element"
             (\element -> all
                 (\(index, row) -> head (nonEmpty row) == toEnum index)
                 (zip [1..(fromEnum element)] (diamond element)))),
         (label "The diamond is the same mirrored"
             (\element -> diamond element == (map reverse $ diamond element))),
         (label "The diamond is the same upside-down"
             (\element -> diamond element == (reverse $ diamond element))),
         (label "The first row has one non-empty cell"
             (\element -> (length $ nonEmpty $ head $ diamond element) == 1)),
         (label "The last row has one non-empty cell"
             (\element -> (length $ nonEmpty $ last $ diamond element) == 1)),
         (label "The first column has one non-empty cell"
             (\element -> (length $ nonEmpty $ map head $ diamond element) == 1)),
         (label "The last column has one non-empty cell"
             (\element -> (length $ nonEmpty $ map last $ diamond element) == 1)),
         (label "All other rows have two non-empty cells"
             (\element -> all (\row -> (length $ nonEmpty row) == 2) (chop $ diamond element))),
         (label "All other columns have two non-empty cells"
             (\element -> all (\column -> (length $ nonEmpty column) == 2) (chop $ transpose $ diamond element)))]

    hspec $ do
        describe "Diamond" $ do
            it "works" $ do
                map (map char) (diamond D) ==
                  ["   A   ",
                   "  B B  ",
                   " C   C ",
                   "D     D",
                   " C   C ",
                   "  B B  ",
                   "   A   "]
