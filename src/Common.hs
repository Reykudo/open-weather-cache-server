{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common where

import Control.Concurrent (MVar)
import Control.Concurrent.STM (TVar)
import Control.Monad.Except (ExceptT (ExceptT), MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (ServerError (ServerError))
import Servant.Client (ClientError)
import Weather (Coords (Coords), Weather)

data Configuration = Configuration
  { port :: Int,
    locations :: [Location],
    timeTolerance :: Maybe Int,
    coordsTolerance :: Maybe Double,
    updatePeriod :: Maybe Int,
    apiKey :: Text,
    apiRoot :: Text
  }
  deriving (Show, Eq, Generic)

data Location = CityId Int | CityNames [Text] | ByCoords Coords | ZipCode [Text] deriving (Show, Read, Eq, Generic)

data AppTContext = AppTContext {cfg :: Configuration, store :: TVar [Weather]}

newtype AppT m a = AppT
  { runAppT :: (ReaderT AppTContext (ExceptT ServerError m)) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader AppTContext,
      MonadError ServerError,
      MonadIO
    )

type App = AppT IO
