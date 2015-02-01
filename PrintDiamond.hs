module Main where

import Diamond

import Control.Monad
import Data.Functor
import System.Environment

main = do
    size <- (read . head) <$> getArgs
    forM_ (diamond size) putStrLn
