# Function to query 10 or 50 stop data for a species in a year
#' @export GetRouteData
#' @importFrom RCurl getURL
#' @importFrom plyr ldply
GetRouteData=function(AOU=NULL, countrynum=NULL, states=NULL, year, weather=NULL, routes=NULL, 
                      Zeroes=TRUE, TenStops = TRUE, 
                      Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/") {
  
  if(TenStops) {
    DirData <- paste0(Dir, "States/")
    CountString <- "^count"
  } else {
    if(any(year<1997)) stop("Data only available from 1997: pre-1997 data not integrated into this function for 50 stop data (yet)")
    DirData <- paste0(Dir, "50-StopData/1997ToPresent_SurveyWide/")
    CountString <- "^stop"
  }
  if(!is.null(countrynum) & any(!(countrynum%in%c(124, 484, 840)))) stop("countrynum should be either 124 (Canada), 484 (Mexico), or 840 (USA)")
  
  GetDat <- function(file, dir, year, AOU, countrynum, states) {
    dat <- GetUnzip(ZipName=paste0(dir, file), FileName=gsub("^Fifty", "fifty", gsub("zip", "csv", file)))
    names(dat) <- tolower(names(dat))
    if(is.null(year)) {  UseYear <- TRUE  } else {  UseYear <- dat$year%in%year  }
    if(is.null(AOU)) {  UseAOU <- TRUE  } else {  UseAOU <- dat$aou%in%AOU  }
    if(is.null(countrynum)) {  UseCountry <- TRUE  } else {  UseCountry <- dat$countrynum%in%countrynum  }
    if(is.null(states)) {  UseState <- TRUE  } else {  UseState <- dat$statenum%in%states  }
    Use <- UseYear & UseAOU & UseCountry & UseState
    if(sum(Use)>0) {
      dat$routeID <- paste(dat$statenum, dat[,grep("^[Rr]oute$", names(dat))])
      dat <-subset(dat, subset=Use)
      return(dat)      
    } else return(NULL)
  }
  
# Only use the files we want
  CountriesToUse <- if(!is.null(countrynum)) {
    RegionsForZipFiles$countrynum%in%countrynum 
  } else {
    TRUE
  }
  StatesToUse <- if(!is.null(states)) {
    RegionsForZipFiles$RegionCode%in%states 
  } else {
    TRUE
  }
  ToUse <- CountriesToUse & StatesToUse
  if(TenStops) {
    Files <- RegionsForZipFiles$FileName10stop[ToUse]
    Missing <- ToUse & is.na(RegionsForZipFiles$FileName10stop)
  } else { # 50 stop
    Files <- RegionsForZipFiles$FileName50stop[ToUse]
    Missing <- ToUse & is.na(RegionsForZipFiles$FileName50stop)
  }
  
  if(length(Files)==0) stop("No data for the states specified")
  if(any(is.na(Files))) warning(paste0("No data for these states: ", paste(RegionsForZipFiles$'State/Prov/TerrName'[Missing], collapse=", ")))
  
  Data.lst <- sapply(Files[!is.na(Files)], GetDat, dir=DirData, year=year, AOU=AOU, countrynum=countrynum, states=states, simplify=FALSE)
  
  if(all(unlist(lapply(Data.lst, is.null)))) {
    warning("no data, sorry")
    AllData <- NULL
  } else {
    Data <- ldply(Data.lst)
# Get route data for all routes, and annual data
    if(is.null(weather)) weather <-GetWeather(Dir)
    if(is.null(year)) {  UseYear <- TRUE  } else {  UseYear <- weather$Year%in%year  }
    if(is.null(countrynum)) {  UseCountry <- TRUE  } else {  UseCountry <- weather$countrynum%in%countrynum  }
    if(is.null(states)) {  UseState <- TRUE  } else {  UseState <- weather$statenum%in%states  }
    UseWeather <- UseYear & UseCountry & UseState
    
    if(is.null(routes)) routes <- GetRoutes(Dir)
    if(is.null(countrynum)) {  UseCountry <- TRUE  } else {  UseCountry <- routes$countrynum%in%countrynum  }
    if(is.null(states)) {  UseState <- TRUE  } else {  UseState <- routes$statenum%in%states  }
    UseRoutes <- UseCountry & UseState
    
    CommonNames <- names(Data)[names(Data)%in%names(weather)]
    CommonNames <- CommonNames[CommonNames%in%names(routes)]
    
    # Subset data
    # First, sites sampled in chosen year(s)
    weather <-subset(weather, subset=UseWeather, 
                     select=c(CommonNames, "Year", "Month", "Day", "RunType"))
    # Route data for sites sampled in chosen years
    routes <- subset(routes, subset=UseRoutes & routes$routeID%in%weather$routeID, 
                     select=c(CommonNames, "Latitude", "Longitude"))
    
  # merge data sets
    dat.routeID.year <- paste(Data$routeID, Data$year, sep=".")
    weather.routeID.year <- paste(weather$routeID, weather$Year, sep=".")
    
    GetID <- function(datID, otherIDs) {
      if(length(datID)!=1) stop("datID should be a scalar")
      wh <- which(datID==otherIDs)
      if(length(wh)!=1) {
        if(length(wh)==0) {
          warning("no ID, so setting to NA")
        } else {
          warning("no unique ID, so using first value")
        }
        wh <- wh[1]
      }
      wh
    }
    WeatherWhiches <- sapply(dat.routeID.year, GetID, otherIDs=weather$routeID.year)
    RouteWhiches <- sapply(Data$routeID, GetID, otherIDs=routes$routeID)
    
    AllData <- cbind(Data, weather[WeatherWhiches, !names(weather)%in%names(Data)],
                     routes[RouteWhiches, !names(routes)%in%names(Data)])
    
  #  if(!is.na(weather)) AllData <- merge(Data, weather, all=TRUE) # by=c("routeID", "RPID"), 
  #  if(!is.na(routes))  AllData <- merge(AllData, routes, all=TRUE) # by="routeID", 
    AllData$SumCount <- apply(AllData[,grep(CountString, names(AllData))],1,sum, na.rm=TRUE)
    if(!Zeroes) AllData <- subset(AllData, AllData$SumCount>0)
    AllData <- AllData[,!names(AllData)%in%c(".id", "routedataid", "year")]
  }

  AllData
}


# Also: add a vars option, to only return some variables
#   Try <- GetRouteData(countrynum=NULL, states=c(89, 40:60), weather=NULL, routes=NULL, AOU=c(4050, 3850), year=2010, Zeroes=FALSE, Dir=NULL)
