# Function to query 50 stop data for a species in a year
#' @export Get50RouteData
Get50RouteData=function(countrynum=NULL, states=NULL, AOU=NULL, year, weather=NULL, routes=NULL, Zeroes=TRUE, 
                        Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/") {
  if(any(year<1997)) stop("Data only available from 1997: pre-1997 data not integrated into this function (yet)")
  if(!is.null(countrynum) & any(!(countrynum%in%c(124, 484, 840)))) stop("countrynum should be either 124 (Canada), 484 (Mexico), or 840 (USA)")
  Dir50 <- paste0(Dir, "50-StopData/1997ToPresent_SurveyWide/")

  GetDat <- function(file, dir, year, AOU, countrynum, states) {
    dat=GetUnzip(ZipName=paste0(dir, file), FileName=gsub("^F", "f", gsub("zip", "csv", file)))
    if(is.null(year)) {  UseYear <- TRUE  } else {  UseYear <- dat$year%in%year  }
    if(is.null(AOU)) {  UseAOU <- TRUE  } else {  UseAOU <- dat$AOU%in%AOU  }
    if(is.null(countrynum)) {  UseCountry <- TRUE  } else {  UseCountry <- dat$countrynum%in%countrynum  }
    if(is.null(states)) {  UseState <- TRUE  } else {  UseState <- dat$statenum%in%states  }
    Use <- UseYear & UseAOU & UseCountry & UseState
    if(sum(Use)>0) {
      dat$routeID=paste(dat$statenum, dat$Route)
      dat=subset(dat, subset=Use)
      return(dat)      
    } else return(NULL)
  }
  
  Data.lst <- sapply(dir(Dir50), GetDat, dir=Dir50, year=year, AOU=AOU, countrynum=countrynum, states=states, simplify=FALSE)
  Data <- plyr::ldply(Data.lst)

  # Get route data for all routes, and annual data
  if(is.null(weather)) weather=GetWeather(Dir)
  names(weather)[names(weather)=="TotalSpp"] <- "SpeciesTotal" # change name of total number of spp to that used in data file
  if(is.null(routes)) routes=GetRoutes()
  if(is.null(year)) {  UseYear <- TRUE  } else {  UseYear <- weather$Year%in%year  }
  if(is.null(countrynum)) {  UseCountry <- TRUE  } else {  UseCountry <- weather$countrynum%in%countrynum  }
  if(is.null(states)) {  UseState <- TRUE  } else {  UseState <- weather$statenum%in%states  }
  UseWeather <- UseYear & UseCountry & UseState
  
  if(is.null(routes)) routes=GetRoutes(Dir)
  if(is.null(countrynum)) {  UseCountry <- TRUE  } else {  UseCountry <- routes$countrynum%in%countrynum  }
  if(is.null(states)) {  UseState <- TRUE  } else {  UseState <- routes$statenum%in%states  }
  UseRoutes <- UseCountry & UseState
  
  # Subset data
  # First, sites sampled in chosen year(s)
  weather=subset(weather, subset=UseWeather, 
                 select=c("countrynum", "statenum", "Year", "Month", "Day", "Route", "RPID", "SpeciesTotal", "RunType", "routeID"))
  # Route data for sites sampled in chosen years
  routes=subset(routes, subset=UseRoutes & routes$routeID%in%weather$routeID, select=c("countrynum", "statenum", "Route", "Lati", "Longi", "routeID"))

  AllData <- merge(Data, weather, all=TRUE) # by="routeID", 
  AllData <- merge(AllData, routes, all=TRUE) # by="routeID", 
  AllData$SumCount <- apply(AllData[,grep("^Stop", names(AllData))],1,sum, na.rm=TRUE)
  if(!Zeroes) AllData <- subset(AllData, AllData$SumCount>0)
  AllData
}


# Should get look-up table to check which states are in which files, & only read them
# Also: add a vars option, to only return some variables
#   Try <- Get50RouteData(countrynum=NULL, states=c(89, 40:60), weather=NULL, routes=NULL, AOU=c(4050, 3850), year=2010, Zeroes=FALSE, Dir=NULL)
