Get10RouteData=function(AOU, spcodes, weather=NULL, routes=NULL, year) {
  #  AOU=SpCodes$AOU[grep("Three", SpCodes$English_Common_Name)]; spcodes=SpCodes; 
  #  weather=NULL; routes=NULL; year=2010:2012
  
  file=spcodes$FileString[spcodes$AOU==AOU]
  dat=GetUnzip(ZipName=paste("ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/",file,".zip",sep=""), 
               FileName=paste(file,".csv",sep=""))
  dat$routeID=paste(dat$statenum, dat$Route)
  names(dat)[names(dat)=="SpeciesTotal"] <- "TotalSpp" # change name of total number of spp to that used in weather file
  # Get route data for all routes
  if(is.null(routes)) routes=GetUnzip(ZipName="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Routes.zip", FileName="routes.csv")
  if(is.null(routes$routeID)) routes$routeID=paste(routes$statenum, routes$Route)
  
  # Annual data
  if(is.null(weather)) weather=GetUnzip(ZipName="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Weather.zip", FileName="weather.csv")
  if(is.null(weather$routeID)) weather$routeID=paste(weather$statenum, weather$Route)
  
  # Subset data
  # First, sites sampled in chosen year(s)
  weather=subset(weather, subset=weather$Year%in%year, 
                 select=c("countrynum", "statenum", "Year", "Month", "Day", "Route", "RPID", "TotalSpp", "RunType", "routeID"))
  # Route data for sites sampled in chosen years
  routes=subset(routes, subset=routes$routeID%in%weather$routeID, select=c("countrynum", "statenum", "Route", "Lati", "Longi", "routeID"))
  # Species occurences in chosen year(s)
  dat=subset(dat, subset=dat$Year%in%year & dat$Aou==AOU, 
             select=c("countrynum", "statenum", "Year", "Route", "count10", "count20", "count30", "count40", "count50", "routeID"))
  
  AllData=merge(dat, weather, all=TRUE) # by="routeID", 
  AllData=merge(AllData, routes, all=TRUE) # by="routeID", 
  AllData$SumCount=apply(AllData[,grep("count[1-5]0", names(AllData))],1,sum, na.rm=TRUE)
  AllData
}


Get10RouteData=function(AOU, spcodes, weather=NULL, routes=NULL, year)
  
  
  AOU=1200; year=2000
if(any(year<1997)) stop("Only data from 1997 available")


RouteIDs.lst <- sapply(1:10, function(ind) {
  dat=GetUnzip(ZipName=paste("ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/50-StopData/1997ToPresent_SurveyWide/Fifty",ind,".zip", sep=""),
                FileName=paste("fifty",ind,".csv",sep=""))
#  cbind(ind=ind,RouteID=unique(paste(dat$statenum, dat$RouteID)))
#  unique(paste(dat$statenum, dat$RouteID))
# paste(dat$statenum, dat$Route)
  unique(dat$statenum)
})
StatesInFile <- data.frame(File=paste("Fifty",rep(1:length(RouteIDs.lst), unlist(lapply(RouteIDs.lst, length))),".zip",sep=""), 
      State=unlist(RouteIDs.lst))

table(StatesInFile$State)


