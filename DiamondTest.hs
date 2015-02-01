module Main where

import Diamond
import Test.Hspec

main = do
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
