module Main where

import Diamond

import Test.Hspec
import Test.QuickCheck

instance Arbitrary DiamondSize where
    arbitrary = elements sizes

main = do
    quickCheck $ (label "The first cell in the biggest row of the diamond is the largest"
                   (\size -> [head (diamond size !! fromEnum size)] == (show size)))

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
