# Function to query 10 stop data for a species in a year
#' @export Get10RouteData
Get10RouteData=function(AOU, weather=NULL, routes=NULL, year, Zeroes=TRUE, vars=NULL, 
                        Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/") {
#  data(SpCodes, envir = environment())

  file=SpCodes$FileString[SpCodes$AOU==AOU]
  dat=GetUnzip(ZipName=paste(Dir, "Species/",file,".zip",sep=""), 
               FileName=paste(file,".csv",sep=""))
  dat$routeID=paste(dat$statenum, dat$Route)
  if(!is.null(vars) & any(!(vars%in%names(dat)))) 
    stop("These variable are not in the data: ", vars[!(vars%in%names(dat))])
  if(is.null(vars)) vars <- names(dat)

  # Get route data for all routes, and annual data
  if(is.null(weather)) weather=GetWeather(Dir)
  names(weather)[names(weather)=="TotalSpp"] <- "SpeciesTotal" # change name of total number of spp to that used in data file
  if(is.null(routes)) routes=GetRoutes(Dir)
  
  # Subset data
  # First, sites sampled in chosen year(s)
  weather=subset(weather, subset=weather$Year%in%year, 
                 select=c("countrynum", "statenum", "Year", "Month", "Day", "Route", "RPID", "SpeciesTotal", "RunType", "routeID"))
  # Route data for sites sampled in chosen years
  routes=subset(routes, subset=routes$routeID%in%weather$routeID, select=c("countrynum", "statenum", "Route", "Lati", "Longi", "routeID"))
  # Species occurences in chosen year(s)
  dat=subset(dat, subset=dat$Year%in%year & dat$Aou==AOU, select=c(vars[!vars%in%c("RPID", "routeID")], "routeID", "RPID"))
  
  AllData=merge(dat, weather, by=c("routeID", "RPID"), all=TRUE) # 
  AllData=merge(AllData, routes, by="routeID", all=TRUE) 
  AllData$SumCount=apply(AllData[,grep("count[1-5]0", names(AllData))],1,sum, na.rm=TRUE)
  if(!Zeroes) AllData=subset(AllData, AllData$SumCount>0)
  return(AllData)
}
