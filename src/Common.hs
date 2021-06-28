{-# LANGUAGE DeriveGeneric #-}
module Common where
import GHC.Generics (Generic)

data Coord = Coord {lon :: Int, lat :: Int} deriving (Show, Read, Eq, Generic)

data Location = CityId String | CityName String | Coords Coord | ZipCode String deriving (Show, Read, Eq, Generic)
