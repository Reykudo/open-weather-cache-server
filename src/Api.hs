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
import Control.Exception (throw, throwIO)
import Control.Monad (forever, guard, void)
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO), mapExceptT, runExceptT, withExceptT)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), ask, asks)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (Foldable (toList), find, foldl')
import Data.LatLong (LatLong (..), geoDistance)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Typeable (Proxy (Proxy))
import GHC.IO (unsafePerformIO)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
import Servant.API
import Servant.Client
import Servant.Server
import Utils (hoistMaybe)
import Weather

type ExternalAPI =
  ( ("weather" :> QueryParam' '[Required] "appid" Text :> QueryParams "q" Text :> Get '[JSON] Weather)
      :<|> ("weather" :> QueryParam' '[Required] "appid" Text :> QueryParam' '[Required] "id" Int :> Get '[JSON] Weather)
      :<|> ("weather" :> QueryParam' '[Required] "appid" Text :> QueryParam' '[Required] "lat" Double :> QueryParam' '[Required] "lon" Double :> Get '[JSON] Weather)
      :<|> ("weather" :> QueryParam' '[Required] "appid" Text :> QueryParams "zip" Text :> Get '[JSON] Weather)
  )

type InternalAPI =
  "weather"
    :> QueryParam "id" Int
    :> QueryParam "lat" Double
    :> QueryParam "lon" Double
    :> QueryParams "q" Text
    :> QueryParams "zip" Text
    :> Get '[JSON] Weather

clientApi :: Proxy ExternalAPI
clientApi = Proxy @ExternalAPI

serverApi :: Proxy InternalAPI
serverApi = Proxy @InternalAPI

getWeatherByCityName :: Text -> [Text] -> ClientM Weather
getWeatherByCityId :: Text -> Int -> ClientM Weather
getWeatherByCoords :: Text -> Double -> Double -> ClientM Weather
getWeatherByZipCode :: Text -> [Text] -> ClientM Weather
getWeatherByCityName :<|> getWeatherByCityId :<|> getWeatherByCoords :<|> getWeatherByZipCode =
  client
    clientApi

runClientMInApp :: Show a => ClientM a -> App a
runClientMInApp clientM = do
  manager <- newTlsManager
  rootPath <- asks $ apiRoot . cfg
  let clientEnv =
        mkClientEnv
          manager
          ( BaseUrl
              { baseUrlScheme = Http,
                baseUrlHost = T.unpack rootPath,
                baseUrlPort = 80,
                baseUrlPath = "/data/2.5"
              }
          )
  response <- liftIO $ runClientM clientM clientEnv

  liftIO $ putStrLn "some request"
  either (throwError . clientErrorToServerError) pure response

getTimeChecker :: AppT IO (Int -> Bool)
getTimeChecker = do
  diff <- asks $ timeTolerance . cfg
  case diff of
    Nothing -> pure $ const False
    Just diff' -> do
      currentTime <- round <$> liftIO getPOSIXTime
      pure (\cacheTime -> abs (cacheTime - currentTime) < diff')

findInCacheBy :: (Weather -> Bool) -> App (Maybe Weather)
findInCacheBy check = do
  cacheMvar <- asks store
  checkTime <- getTimeChecker
  v <- liftIO $ readMVar cacheMvar
  liftIO $ print $ length v
  let finder weather@Weather {nameWeather, dtWeather} = check weather && checkTime dtWeather
  liftIO $ withMVar cacheMvar (pure . find finder)

findInCacheByCoords :: Coord -> AppT IO (Maybe Weather)
findInCacheByCoords targetCoord@Coord {lonCoord = targetLon, latCoord = targetLat} = do
  coordsTollerance <- asks $ coordTolerance . cfg
  let targetLatLong = LatLong targetLat targetLon
  checker <- case coordsTollerance of
    Nothing -> pure ((== targetCoord) . coordWeather)
    Just coordTolerance' -> pure (\Weather {coordWeather = Coord {lonCoord, latCoord}} -> geoDistance (LatLong latCoord lonCoord) targetLatLong <= coordTolerance')

  findInCacheBy checker

saveToCache :: Weather -> App ()
saveToCache weather = do
  cacheMvar <- asks store
  liftIO $ modifyMVar_ cacheMvar (pure . (weather :))

withApiKey :: MonadReader AppTContext m => (Text -> b) -> m b
withApiKey method = do
  apiKey <- asks (apiKey . cfg)
  pure $ method apiKey

handleGetWeatherByCityName :: [Text] -> App Weather
handleGetWeatherByCityName args = do
  let cityName = head args
  valueFromCahce <- findInCacheBy ((== cityName) . nameWeather)
  case valueFromCahce of
    Nothing -> do
      method <- withApiKey getWeatherByCityName
      weather <- runClientMInApp $ method args
      saveToCache weather
      pure weather
    Just w -> pure w

handleGetWeatherByCityId :: Int -> AppT IO Weather
handleGetWeatherByCityId cityId = do
  valueFromCahce <- findInCacheBy ((== cityId) . weatherIDWeather)
  case valueFromCahce of
    Nothing -> do
      method <- withApiKey getWeatherByCityId
      weather <- runClientMInApp $ method cityId
      saveToCache weather
      pure weather
    Just w -> pure w

handleGetWeatherByCoords :: Double -> Double -> AppT IO Weather
handleGetWeatherByCoords lat lon = do
  valueFromCahce <- findInCacheByCoords (Coord {latCoord = lat, lonCoord = lon})
  case valueFromCahce of
    Nothing -> do
      method <- withApiKey getWeatherByCoords
      weather <- runClientMInApp $ method lat lon
      saveToCache weather
      pure weather
    Just w -> pure w

handleGetWeatherByZipCode :: [Text] -> AppT IO Weather
handleGetWeatherByZipCode args = do
  method <- withApiKey getWeatherByZipCode
  weather <- runClientMInApp $ method args
  saveToCache weather
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

handlers :: ServerT InternalAPI App
handlers = handleGetWeather

clientErrorToServerError :: ClientError -> ServerError
clientErrorToServerError
  (FailureResponse _ Response {responseStatusCode = status, responseHeaders, responseBody}) =
    ServerError {errHTTPCode = statusCode status, errBody = responseBody, errHeaders = toList responseHeaders, errReasonPhrase = "FailureResponse"}
clientErrorToServerError _ = err400

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
                          saveToCache weather
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
