module Main where

import Configuration.Configuration (getConfig)
import Control.Monad (void)
import Lib

main :: IO ()
main = print =<< getConfig
