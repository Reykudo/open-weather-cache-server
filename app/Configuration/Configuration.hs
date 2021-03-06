{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Configuration.Configuration (getConfig) where

import Common
import Configuration.Dhall (getConfigFromDhall)
import Configuration.Envs (getConfigFromEnvs)
import Configuration.Options (getConfigFromOptions)
import Control.Exception (SomeException (SomeException), throw, throwIO)
import Control.Monad.Trans.Except (runExcept, runExceptT)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Foldable (asum)
import Text.Read (readMaybe)

getConfig :: IO Configuration
getConfig = do
  cfgFromDhal <- runExceptT $ asum [getConfigFromOptions, getConfigFromDhall, getConfigFromEnvs]

  case cfgFromDhal of
    Left s -> throwIO $ userError s
    Right r -> pure r
