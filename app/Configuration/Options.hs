{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Configuration.Options (getConfigFromOptions) where

import Common (Configuration (..))
import Control.Exception
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.Text (Text)
import Env.Generic (type (?) (Help))
import Options.Applicative
import Text.Read (readMaybe)
import Weather (Coord (Coord))

getConfigFromOptions :: (ExceptT String IO) Configuration
getConfigFromOptions =
  --  FIXME убрать вывод ошибок в консоль. Вместо этого выводить в Left
  ExceptT $ (Right <$> parse') `catch` (pure . Left . show @SomeException)
  where
    parse' =
      execParser $
        info
          (helper <*> options)
          ( fullDesc
              <> progDesc "Cache server for OpenWeather"
          )
    options =
      Configuration
        <$> option
          (auto)
          ( long "port"
              <> short 'p'
              <> metavar "PORT"
              <> help "Port"
          )
          <*> option
            (auto)
            ( long "locations"
                <> short 'l'
                <> metavar "LOCATIONS"
                <> help "Locations"
                <> value []
            )
          <*> option
            (maybeReader (Just . (readMaybe @Int)))
            ( long "timeTolerance"
                <> short 't'
                <> metavar "TIME"
                <> help "Time tolerance"
                <> value Nothing
            )
          <*> option
            (maybeReader (Just . (readMaybe @Double)))
            ( long "coordTolerance"
                <> short 'c'
                <> metavar "COORD"
                <> help "Coordinates tolerance"
                <> value Nothing
            )
          <*> option
            (maybeReader (Just . (readMaybe @Int)))
            ( long "updatePeriod"
                <> short 'u'
                <> metavar "UPDATE"
                <> help "Update period"
                <> value Nothing
            )
          <*> option
            (auto)
            ( long "apiKey"
                <> short 'k'
                <> metavar "API_KEY"
                <> help "Api key"
            )
          <*> option
            (auto)
            ( long "apiRoot"
                <> short 'r'
                <> metavar "API_ROOT"
                <> help "Api Root"
                <> value ("api.openweathermap.org")
            )
