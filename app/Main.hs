module Main where

import Configuration.Configuration (getConfig)
import Control.Monad (void)
import Lib (startServer)

main :: IO ()
main = do
  cfg <- getConfig
  print cfg
  startServer cfg
