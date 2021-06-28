{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}

module Configuration.Options (getConfigFromOptions) where

import Common (Coord (Coord))
import Configuration.Common (Configuration (Configuration))
import Control.Exception
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Env.Generic (type (?) (Help))
import Options.Applicative
import Text.Read (readMaybe)

getConfigFromOptions :: (ExceptT String IO) Configuration
getConfigFromOptions =
  --  TODO вывод ошибки в Either
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
            (maybeReader (Just . (readMaybe @Coord)))
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
