{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Configuration.Common where

import Common (Coord, Location)
import Env.Generic ( type (?) )
import qualified Env.Generic as Env

data Configuration = Configuration
  { port :: Int,
    locations :: [Location],
    timeTolerance :: Maybe Int,
    coordTolerance :: Maybe Coord,
    updatePeriod :: Maybe Int
  }
  deriving (Show, Eq, Env.Generic)
