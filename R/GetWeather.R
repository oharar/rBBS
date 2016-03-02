# Get Weather data (i.e. which routes were surveyed each year)
#' @export GetWeather
GetWeather <- function(Dir=NULL) {
  if(is.null(Dir)) Dir <- "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/"
  weather=GetUnzip(ZipName=paste0(Dir, "Weather.zip"), FileName="weather.csv")
  weather$routeID=paste(weather$statenum, weather$Route)
  weather  
}
