{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Client.OpenWeather where

import Common (App, AppTContext (cfg), Configuration (apiRoot))
import Control.Monad.Reader (MonadIO (liftIO), asks)
import Data.Foldable (Foldable (toList))
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Status (statusCode))
import Servant
import Servant.API (type (:<|>) (..))
import Servant.Client
import Weather (Weather (Weather))

getWeatherByCityName :: Text -> [Text] -> ClientM Weather
getWeatherByCityId :: Text -> Int -> ClientM Weather
getWeatherByCoords :: Text -> Double -> Double -> ClientM Weather
getWeatherByZipCode :: Text -> [Text] -> ClientM Weather
getWeatherByCityName :<|> getWeatherByCityId :<|> getWeatherByCoords :<|> getWeatherByZipCode =
  client
    clientApi

type OpenWeatherAPI =
  ( ("weather" :> QueryParam' '[Required] "appid" Text :> QueryParams "q" Text :> Get '[JSON] Weather)
      :<|> ("weather" :> QueryParam' '[Required] "appid" Text :> QueryParam' '[Required] "id" Int :> Get '[JSON] Weather)
      :<|> ("weather" :> QueryParam' '[Required] "appid" Text :> QueryParam' '[Required] "lat" Double :> QueryParam' '[Required] "lon" Double :> Get '[JSON] Weather)
      :<|> ("weather" :> QueryParam' '[Required] "appid" Text :> QueryParams "zip" Text :> Get '[JSON] Weather)
  )

clientApi :: Proxy OpenWeatherAPI
clientApi = Proxy @OpenWeatherAPI

clientErrorToServerError :: ClientError -> ServerError
clientErrorToServerError
  (FailureResponse _ Response {responseStatusCode = status, responseHeaders, responseBody}) =
    ServerError {errHTTPCode = statusCode status, errBody = responseBody, errHeaders = toList responseHeaders, errReasonPhrase = "FailureResponse"}
clientErrorToServerError _ = err400

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
