{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib (startServer) where

import Api (initServer, startSheduler)
import Common
import Control.Concurrent (MVar, forkIO, newMVar)
import Control.Concurrent.STM (atomically, newTVar, newTVarIO)
import Control.Monad (void)
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS (RWST (RWST))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import qualified Data.Text as T
import Network.Wai.Handler.Warp (run)
import Servant.Client (ClientError)

startServer :: Configuration -> IO ()
startServer cfg = do
  store_ <- newTVarIO []
  let appPort = port cfg
  liftIO $ putStrLn $ "starting server at port " <> show appPort
  let appContext = AppTContext {cfg, store = store_}
  forkIO $ void $ runExceptT $ runAppT startSheduler `runReaderT` appContext
  run appPort $ initServer appContext
