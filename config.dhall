-- ./config.dhall                               
let Location : Type = < CityId : Integer | CityNames : List Text | ByCoords : { lonCoord : Double, latCoord : Double } | ZipCode : List Text>
in { port  = +8080
    , locations  = [Location.CityNames ["London"], Location.ByCoords { lonCoord =74.59, latCoord = 42.87 }] : List Location
    , timeTolerance = Some +5000
    , coordsTolerance = None  Double
    , updatePeriod = Some +4000
    , apiKey = "647a6995c865b21f50d76f7cdf3cc92c"
    , apiRoot = "api.openweathermap.org"}