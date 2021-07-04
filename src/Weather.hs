{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Weather where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data Weather = Weather
  { coordWeather :: Coord,
    weatherWeather :: [WeatherElement],
    baseWeather :: Text,
    mainWeather :: Main,
    visibilityWeather :: Int,
    windWeather :: Wind,
    cloudsWeather :: Clouds,
    dtWeather :: Int,
    sysWeather :: Sys,
    timezoneWeather :: Int,
    weatherIDWeather :: Int,
    nameWeather :: Text,
    codWeather :: Int
  }
  deriving (Show)

newtype Clouds = Clouds
  { allClouds :: Int
  }
  deriving (Show)

data Coord = Coord
  { lonCoord :: Double,
    latCoord :: Double
  }
  deriving (Show, Eq, Read, Generic)

data Main = Main
  { tempMain :: Double,
    feelsLikeMain :: Double,
    tempMinMain :: Double,
    tempMaxMain :: Double,
    pressureMain :: Int,
    humidityMain :: Int
  }
  deriving (Show)

data Sys = Sys
  { sysTypeSys :: Int,
    sysIDSys :: Int,
    countrySys :: Text,
    sunriseSys :: Int,
    sunsetSys :: Int
  }
  deriving (Show)

data WeatherElement = WeatherElement
  { weatherIDWeatherElement :: Int,
    mainWeatherElement :: Text,
    descriptionWeatherElement :: Text,
    iconWeatherElement :: Text
  }
  deriving (Show)

data Wind = Wind
  { speedWind :: Double,
    degWind :: Int
  }
  deriving (Show)

-- decodeTopLevel :: ByteString -> Maybe Weather
-- decodeTopLevel = decode

instance ToJSON Weather where
  toJSON (Weather coordWeather weatherWeather baseWeather mainWeather visibilityWeather windWeather cloudsWeather dtWeather sysWeather timezoneWeather weatherIDWeather nameWeather codWeather) =
    object
      [ "coord" .= coordWeather,
        "weather" .= weatherWeather,
        "base" .= baseWeather,
        "main" .= mainWeather,
        "visibility" .= visibilityWeather,
        "wind" .= windWeather,
        "clouds" .= cloudsWeather,
        "dt" .= dtWeather,
        "sys" .= sysWeather,
        "timezone" .= timezoneWeather,
        "id" .= weatherIDWeather,
        "name" .= nameWeather,
        "cod" .= codWeather
      ]

instance FromJSON Weather where
  parseJSON (Object v) =
    Weather
      <$> v .: "coord"
      <*> v .: "weather"
      <*> v .: "base"
      <*> v .: "main"
      <*> v .: "visibility"
      <*> v .: "wind"
      <*> v .: "clouds"
      <*> v .: "dt"
      <*> v .: "sys"
      <*> v .: "timezone"
      <*> v .: "id"
      <*> v .: "name"
      <*> v .: "cod"

instance ToJSON Clouds where
  toJSON (Clouds allClouds) =
    object
      [ "all" .= allClouds
      ]

instance FromJSON Clouds where
  parseJSON (Object v) =
    Clouds
      <$> v .: "all"

instance ToJSON Coord where
  toJSON (Coord lonCoord latCoord) =
    object
      [ "lon" .= lonCoord,
        "lat" .= latCoord
      ]

instance FromJSON Coord where
  parseJSON (Object v) =
    Coord
      <$> v .: "lon"
      <*> v .: "lat"

instance ToJSON Main where
  toJSON (Main tempMain feelsLikeMain tempMinMain tempMaxMain pressureMain humidityMain) =
    object
      [ "temp" .= tempMain,
        "feels_like" .= feelsLikeMain,
        "temp_min" .= tempMinMain,
        "temp_max" .= tempMaxMain,
        "pressure" .= pressureMain,
        "humidity" .= humidityMain
      ]

instance FromJSON Main where
  parseJSON (Object v) =
    Main
      <$> v .: "temp"
      <*> v .: "feels_like"
      <*> v .: "temp_min"
      <*> v .: "temp_max"
      <*> v .: "pressure"
      <*> v .: "humidity"

instance ToJSON Sys where
  toJSON (Sys sysTypeSys sysIDSys countrySys sunriseSys sunsetSys) =
    object
      [ "type" .= sysTypeSys,
        "id" .= sysIDSys,
        "country" .= countrySys,
        "sunrise" .= sunriseSys,
        "sunset" .= sunsetSys
      ]

instance FromJSON Sys where
  parseJSON (Object v) =
    Sys
      <$> v .: "type"
      <*> v .: "id"
      <*> v .: "country"
      <*> v .: "sunrise"
      <*> v .: "sunset"

instance ToJSON WeatherElement where
  toJSON (WeatherElement weatherIDWeatherElement mainWeatherElement descriptionWeatherElement iconWeatherElement) =
    object
      [ "id" .= weatherIDWeatherElement,
        "main" .= mainWeatherElement,
        "description" .= descriptionWeatherElement,
        "icon" .= iconWeatherElement
      ]

instance FromJSON WeatherElement where
  parseJSON (Object v) =
    WeatherElement
      <$> v .: "id"
      <*> v .: "main"
      <*> v .: "description"
      <*> v .: "icon"

instance ToJSON Wind where
  toJSON (Wind speedWind degWind) =
    object
      [ "speed" .= speedWind,
        "deg" .= degWind
      ]

instance FromJSON Wind where
  parseJSON (Object v) =
    Wind
      <$> v .: "speed"
      <*> v .: "deg"
