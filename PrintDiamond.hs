#!/usr/bin/env runhaskell

module Main where

import Diamond

import Control.Monad
import Data.Functor
import System.Environment

main = do
    element <- (read . head) <$> getArgs
    forM_ (diamond element) putStrLn
