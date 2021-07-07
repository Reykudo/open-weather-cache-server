{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Common (App, AppT (runAppT), AppTContext (cfg, store), Configuration (apiKey, apiRoot, coordTolerance, locations, timeTolerance, updatePeriod), Location (CityId, CityNames, Coords, ZipCode))
import Control.Concurrent (modifyMVar, modifyMVar_, putMVar, readMVar, takeMVar, threadDelay, withMVar)
import Control.Concurrent.Async (forConcurrently, forConcurrently_)
import Control.Concurrent.STM (STM, atomically, modifyTVar, readTVar)
import Control.Exception (throw, throwIO)
import Control.Monad (forever, guard, void)
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO), mapExceptT, runExceptT, withExceptT)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (find)
import Data.LatLong (LatLong (..), geoDistance)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Typeable (Proxy (Proxy))
import Client.OpenWeather
import Servant.API
import Servant.Server
import Utils (hoistMaybe)
import Weather

type AppAPI =
  "weather"
    :> QueryParam "id" Int
    :> QueryParam "lat" Double
    :> QueryParam "lon" Double
    :> QueryParams "q" Text
    :> QueryParams "zip" Text
    :> Get '[JSON] Weather

serverApi :: Proxy AppAPI
serverApi = Proxy @AppAPI

getTimeChecker :: AppT IO (Int -> Bool)
getTimeChecker = do
  diff <- asks $ timeTolerance . cfg
  case diff of
    Nothing -> pure $ const False
    Just diff' -> do
      currentTime <- round <$> liftIO getPOSIXTime
      pure (\cacheTime -> abs (cacheTime - currentTime) < diff')

findInCacheBy :: (Int -> Bool) -> (Weather -> Bool) -> ReaderT AppTContext STM (Maybe Weather)
findInCacheBy checkTime check = do
  cacheTvar <- asks store
  cahce <- lift $ readTVar cacheTvar
  let finder weather@Weather {nameWeather, dtWeather} = check weather && checkTime dtWeather
  pure $ find finder cahce

findInCacheByCoords :: (Int -> Bool) -> Coord -> ReaderT AppTContext STM (Maybe Weather)
findInCacheByCoords checkTime targetCoord@Coord {lonCoord = targetLon, latCoord = targetLat} = do
  coordsTollerance <- asks $ coordTolerance . cfg
  let targetLatLong = LatLong targetLat targetLon
  checker <- case coordsTollerance of
    Nothing -> pure ((== targetCoord) . coordWeather)
    Just coordTolerance' -> pure (\Weather {coordWeather = Coord {lonCoord, latCoord}} -> geoDistance (LatLong latCoord lonCoord) targetLatLong <= coordTolerance')

  findInCacheBy checkTime checker

-- saveToCache :: Weather -> App ()
saveToCache :: Weather -> ReaderT AppTContext STM ()
saveToCache weather = do
  cacheTvar <- asks store
  lift $ modifyTVar cacheTvar (weather :)

withApiKey :: MonadReader AppTContext m => (Text -> b) -> m b
withApiKey method = do
  apiKey <- asks (apiKey . cfg)
  pure $ method apiKey

handleGetWeatherByCityName :: [Text] -> App Weather
handleGetWeatherByCityName args = do
  let cityName = head args
  appContext <- ask
  let withAppContext = flip runReaderT appContext
  timeChecker <- getTimeChecker
  valueFromCahce <- liftIO $ atomically $ withAppContext (findInCacheBy timeChecker ((== cityName) . nameWeather))
  case valueFromCahce of
    Nothing -> do
      method <- withApiKey getWeatherByCityName
      weather <- runClientMInApp $ method args
      liftIO $ atomically $ withAppContext (saveToCache weather)
      pure weather
    Just w -> pure w

handleGetWeatherByCityId :: Int -> AppT IO Weather
handleGetWeatherByCityId cityId = do
  timeChecker <- getTimeChecker

  appContext <- ask
  let withAppContext = flip runReaderT appContext
  valueFromCahce <- liftIO $ atomically $ withAppContext (findInCacheBy timeChecker ((== cityId) . weatherIDWeather))
  case valueFromCahce of
    Nothing -> do
      method <- withApiKey getWeatherByCityId
      weather <- runClientMInApp $ method cityId
      liftIO $ atomically $ withAppContext $ saveToCache weather
      pure weather
    Just w -> pure w

handleGetWeatherByCoords :: Double -> Double -> AppT IO Weather
handleGetWeatherByCoords lat lon = do
  timeChecker <- getTimeChecker
  appContext <- ask
  let withAppContext = flip runReaderT appContext

  valueFromCahce <- liftIO $ atomically $ runReaderT (findInCacheByCoords timeChecker (Coord {latCoord = lat, lonCoord = lon})) appContext
  case valueFromCahce of
    Nothing -> do
      method <- withApiKey getWeatherByCoords
      weather <- runClientMInApp $ method lat lon
      liftIO $ atomically $ withAppContext $ saveToCache weather
      pure weather
    Just w -> pure w

handleGetWeatherByZipCode :: [Text] -> AppT IO Weather
handleGetWeatherByZipCode args = do
  appContext <- ask
  let withAppContext = flip runReaderT appContext

  method <- withApiKey getWeatherByZipCode
  weather <- runClientMInApp $ method args
  liftIO $ atomically $ withAppContext $ saveToCache weather
  pure weather

handleGetWeather ::
  Maybe Int ->
  Maybe Double ->
  Maybe Double ->
  [Text] ->
  [Text] ->
  AppT IO Weather
handleGetWeather (Just idParam) _ _ _ _ = handleGetWeatherByCityId idParam
handleGetWeather _ (Just latParam) (Just lonParam) _ _ = handleGetWeatherByCoords latParam lonParam
handleGetWeather _ _ _ qParams@[_] _ = handleGetWeatherByCityName qParams
handleGetWeather _ _ _ _ zipParams@[_] = handleGetWeatherByZipCode zipParams
handleGetWeather _ _ _ _ _ = throwError err400 {errBody = "query params not valid"}

handlers :: ServerT AppAPI App
handlers = handleGetWeather

getByLocation :: Location -> AppT IO Weather
getByLocation (CityId cityId) = do
  method <- withApiKey getWeatherByCityId
  runClientMInApp $ method cityId
getByLocation (CityNames cityNames) = do
  method <- withApiKey getWeatherByCityName
  runClientMInApp $ method cityNames
getByLocation (Coords Coord {latCoord, lonCoord}) = do
  method <- withApiKey getWeatherByCoords
  runClientMInApp $ method latCoord lonCoord
getByLocation (ZipCode zipCode) = do
  method <- withApiKey getWeatherByZipCode
  runClientMInApp $ method zipCode

startSheduler :: App ()
startSheduler = void $
  runMaybeT $ do
    appContext <- ask
    let config = cfg appContext
    delay <- hoistMaybe $ (* 1000) <$> updatePeriod config
    let locs = locations config
    guard $ not $ null locs
    forever $
      liftIO $ do
        threadDelay delay
        forConcurrently_
          locs
          ( \loc -> do
              res <-
                runExceptT $
                  (`runReaderT` appContext) $
                    runAppT
                      ( do
                          weather <- getByLocation loc
                          appContext <- ask
                          let withAppContext = flip runReaderT appContext
                          liftIO $ atomically $ withAppContext $ saveToCache weather
                          pure weather
                      )

              case res of
                Left e -> putStrLn $ "An error has occurance " <> show e
                Right _ -> pure ()
          )

initServer :: AppTContext -> Application
initServer appContext =
  serve serverApi $
    hoistServer
      serverApi
      ( \m -> do
          Handler $ runReaderT (runAppT m) appContext
      )
      handlers
