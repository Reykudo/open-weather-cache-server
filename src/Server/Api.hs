{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Api where

import Client.OpenWeather
import Common (App, AppT (runAppT), AppTContext (AppTContext, cfg, store), Configuration (Configuration, apiKey, apiRoot, coordsTolerance, locations, timeTolerance, updatePeriod), Location (ByCoords, CityId, CityNames, ZipCode))
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

data FindContext = FindContext {appContext :: AppTContext, timeChecker :: Int -> Bool}

findInCacheBy :: (Weather -> Bool) -> ReaderT FindContext STM (Maybe Weather)
findInCacheBy check = do
  FindContext {appContext = AppTContext {store}, timeChecker} <- ask
  cache <- lift $ readTVar store
  let finder weather@Weather {nameWeather, dtWeather} = check weather && timeChecker dtWeather
  pure $ find finder cache

findInCacheByCoords :: Coords -> ReaderT FindContext STM (Maybe Weather)
findInCacheByCoords targetCoord@Coords {lonCoord = targetLon, latCoord = targetLat} = do
  FindContext {appContext = AppTContext {cfg = Configuration {coordsTolerance}}} <- ask
  let targetLatLong = LatLong targetLat targetLon
  checker <- case coordsTolerance of
    Nothing -> pure ((== targetCoord) . coordWeather)
    Just coordsTolerance' -> pure (\Weather {coordWeather = Coords {lonCoord, latCoord}} -> geoDistance (LatLong latCoord lonCoord) targetLatLong <= coordsTolerance')

  findInCacheBy checker

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

  timeChecker <- getTimeChecker
  let withFindContext = flip runReaderT $ FindContext appContext timeChecker
  let withAppContext = flip runReaderT appContext
  valueFromCache <- liftIO $ atomically $ withFindContext (findInCacheBy ((== cityName) . nameWeather))
  case valueFromCache of
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
  let withFindContext = flip runReaderT $ FindContext appContext timeChecker
  let withAppContext = flip runReaderT appContext
  valueFromCache <- liftIO $ atomically $ withFindContext (findInCacheBy ((== cityId) . weatherIDWeather))
  case valueFromCache of
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
  let withFindContext = flip runReaderT $ FindContext appContext timeChecker
  let withAppContext = flip runReaderT appContext

  valueFromCache <- liftIO $ atomically $ withFindContext (findInCacheByCoords (Coords {latCoord = lat, lonCoord = lon}))
  case valueFromCache of
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
getByLocation (ByCoords Coords {latCoord, lonCoord}) = do
  method <- withApiKey getWeatherByCoords
  runClientMInApp $ method latCoord lonCoord
getByLocation (ZipCode zipCode) = do
  method <- withApiKey getWeatherByZipCode
  runClientMInApp $ method zipCode

startScheduler :: App ()
startScheduler = void $
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
                          let withFindContext = flip runReaderT appContext
                          liftIO $ atomically $ withFindContext $ saveToCache weather
                          pure weather
                      )

              case res of
                Left e -> putStrLn $ "An error has occurrence " <> show e
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
