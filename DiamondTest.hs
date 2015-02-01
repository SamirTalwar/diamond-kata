module Main where

import Diamond

import Control.Monad
import Data.Char (chr, ord)
import Test.Hspec
import Test.QuickCheck

instance Arbitrary Element where
    arbitrary = elements sizes

nonEmpty = filter (/= ' ')

main = do
    mapM_ quickCheck
        [(label "The diamond has 2*N-1 rows, where N is the size"
             (\size -> (length $ diamond size) == fromEnum size * 2 - 1)),
         (label "The diamond has 2*N-1 columns, where N is the size"
             (\size -> all (\row -> length row == fromEnum size * 2 - 1) (diamond size))),
         (label "The diamond's rows go from A to N, where N is the size"
             (\size -> all
                 (\(index, row) -> head (nonEmpty row) == (chr (ord 'A' + index)))
                 (zip [0..(fromEnum size - 1)] (diamond size)))),
         (label "The diamond is the same mirrored"
             (\size -> diamond size == (map reverse $ diamond size))),
         (label "The diamond is the same upside-down"
             (\size -> diamond size == (reverse $ diamond size))),
         (label "The first row has one non-empty cell"
             (\size -> (length $ nonEmpty $ head $ diamond size) == 1)),
         (label "The last row has one non-empty cell"
             (\size -> (length $ nonEmpty $ last $ diamond size) == 1)),
         (label "The first column has one non-empty cell"
             (\size -> (length $ nonEmpty $ (map head) $ diamond size) == 1)),
         (label "The last column has one non-empty cell"
             (\size -> (length $ nonEmpty $ (map last) $ diamond size) == 1))]

    hspec $ do
        describe "Diamond" $ do
            it "works" $ do
                diamond D ==
                  ["   A   ",
                   "  B B  ",
                   " C   C ",
                   "D     D",
                   " C   C ",
                   "  B B  ",
                   "   A   "]

        where
        nonEmpty = filter (/= ' ')
