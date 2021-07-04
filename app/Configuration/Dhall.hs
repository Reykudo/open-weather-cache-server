{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Configuration.Dhall (getConfigFromDhall) where

import Common (Configuration, Location)
import Control.Exception (SomeException, catch, try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Dhall
import Weather (Coord (Coord))

-- instance Generic Location

instance FromDhall Coord

instance FromDhall Location

instance FromDhall Configuration

getConfigFromDhall :: ExceptT String IO Configuration
getConfigFromDhall = do
  ExceptT $ catch (input (Right <$> auto) "./config.dhall") (pure . Left . show @SomeException)
