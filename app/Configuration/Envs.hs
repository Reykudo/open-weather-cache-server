{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Configuration.Envs (getConfigFromEnvs) where

import Common (Coord, Location)
import Configuration.Common (Configuration (Configuration))
import Control.Arrow (left)
import Control.Exception (SomeException, catch, try)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Env
import Text.Read (readEither, readMaybe)

parseField :: (Read a) => String -> Either Error a
parseField = left UnreadError . readEither

-- getConfigFromEnvs :: ExceptT String IO Configuration
getConfigFromEnvs :: (ExceptT String IO) Configuration
getConfigFromEnvs = ExceptT $ (Right <$> parse') `catch` (pure . Left . show @SomeException)
  where
    parse' =
      parse (header "Cache server for OpenWeather") $
        Configuration <$> var parseField "PORT" (help "Target for the greeting")
          <*> var parseField "LOCATIONS" (help "Locations" <> def [])
          <*> var parseField "TIME" (help "Time tolerance" <> def Nothing)
          <*> var parseField "COORD" (help "Coordinates tolerance" <> def Nothing)
          <*> var parseField "UPDATE" (help "Update period" <> def Nothing)
