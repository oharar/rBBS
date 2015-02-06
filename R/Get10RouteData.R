# Function to query 10 stop data for a species in a year
Get10RouteData=function(AOU, weather=NULL, routes=NULL, year, Zeroes=TRUE) {
  data(SpCodes)
  #  AOU=SpCodes$AOU[grep("Three", SpCodes$EnglishFull)]; 
  #  weather=NULL; routes=NULL; year=2010:2012
  
  file=SpCodes$FileString[SpCodes$AOU==AOU]
  dat=GetUnzip(ZipName=paste("ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/",file,".zip",sep=""), 
               FileName=paste(file,".csv",sep=""))
  dat$routeID=paste(dat$statenum, dat$Route)
  names(dat)[names(dat)=="SpeciesTotal"] <- "TotalSpp" # change name of total number of spp to that used in weather file
  # Get route data for all routes, and annual data
  if(is.null(weather)) weather=GetWeather()
  if(is.null(routes)) routes=GetRoutes()
  
  # Subset data
  # First, sites sampled in chosen year(s)
  weather=subset(weather, subset=weather$Year%in%year, 
                 select=c("countrynum", "statenum", "Year", "Month", "Day", "Route", "RPID", "TotalSpp", "RunType", "routeID"))
  # Route data for sites sampled in chosen years
  routes=subset(routes, subset=routes$routeID%in%weather$routeID, select=c("countrynum", "statenum", "Route", "Lati", "Longi", "routeID"))
  # Species occurences in chosen year(s)
  dat=subset(dat, subset=dat$Year%in%year & dat$Aou==AOU, 
             select=c("countrynum", "statenum", "Year", "Route", "count10", "count20", "count30", "count40", "count50", "routeID"))
  
  AllData <- merge(dat, weather, all=TRUE) # by="routeID", 
  AllData <- merge(AllData, routes, all=TRUE) # by="routeID", 
  AllData$SumCount <- apply(AllData[,grep("count[1-5]0", names(AllData))],1,sum, na.rm=TRUE)
  if(!Zeroes) AllData <- subset(AllData, AllData$SumCount>0)
  AllData
}
