module Main where

import Diamond

import Control.Monad
import Test.Hspec
import Test.QuickCheck

instance Arbitrary DiamondSize where
    arbitrary = elements sizes

main = do
    mapM_ quickCheck
        [(label "The first cell in the biggest row of the diamond is the largest"
             (\size -> [head (diamond size !! (fromEnum size - 1))] == (show size))),
         (label "The diamond has 2*N-1 rows, where N is the size"
             (\size -> (length $ diamond size) == fromEnum size * 2 - 1)),
         (label "The diamond has 2*N-1 columns, where N is the size"
             (\size -> all (\row -> length row == fromEnum size * 2 - 1) (diamond size)))]

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
