# Get Weather data (i.e. which routes were surveyed each year)
GetWeather <- function() {
  weather=GetUnzip(ZipName="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Weather.zip", FileName="weather.csv")
  weather$routeID=paste(weather$statenum, weather$Route)
  weather  
}
