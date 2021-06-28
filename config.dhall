-- ./config.dhall
{ port  = +5
 , locations  = [] : List
       < CityId : Text
       | CityName : Text
       | Coords : { lon : Integer, lat : Integer }
       | ZipCode : Text
       >
     , timeTolerance = None  Integer
 , coordTolerance = None  { lon : Integer, lat : Integer }
 , updatePeriod = None  Integer
 }  