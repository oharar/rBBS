# Function to query 10 stop data for a species in a year
#' @export Get10RouteData
#' @importFrom RCurl getURL
#' @importFrom plyr ldply
Get10RouteData <- function(AOU=NULL, countrynum=NULL, states=NULL, year, weather=NULL, routes=NULL, Zeroes=TRUE, 
                        Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/") {
  if(!is.null(countrynum) & any(!(countrynum%in%c(124, 484, 840)))) stop("countrynum should be either 124 (Canada), 484 (Mexico), or 840 (USA)")
  Dir10 <- paste0(Dir, "States/")
  
  GetDat <- function(file, dir, year, AOU, countrynum, states) {
    dat=GetUnzip(ZipName=paste0(dir, file), FileName=gsub("zip", "csv", file))
    if(is.null(year)) {  UseYear <- TRUE  } else {  UseYear <- dat$Year%in%year  }
    if(is.null(AOU)) {  UseAOU <- TRUE  } else {  UseAOU <- dat$Aou%in%AOU  }
    if(is.null(countrynum)) {  UseCountry <- TRUE  } else {  UseCountry <- dat$countrynum%in%countrynum  }
    if(is.null(states)) {  UseState <- TRUE  } else {  UseState <- dat$statenum%in%states  }
    Use <- UseYear & UseAOU & UseCountry & UseState
    if(sum(Use)>0) {
      dat$routeID=paste(dat$statenum, dat$Route)
      dat=subset(dat, subset=Use)
      return(dat)      
    } else return(NULL)
  }
  
  if(grepl("^ftp", Dir10)) {
    Files <- strsplit(getURL(Dir10, ftp.use.epsv = FALSE, dirlistonly = TRUE, crlf=TRUE),"\n")[[1]]
  } else {
    Files <- dir(Dir10)
  }
  Files <- Files[grep("\\.zip$", Files)]
  Data.lst <- sapply(Files, GetDat, dir=Dir10, year=year, AOU=AOU, countrynum=countrynum, states=states, simplify=FALSE)
  Data <- ldply(Data.lst)
  
  # Get route data for all routes, and annual data
  if(is.null(weather)) weather=GetWeather(Dir)
  if(is.null(year)) {  UseYear <- TRUE  } else {  UseYear <- weather$Year%in%year  }
  if(is.null(countrynum)) {  UseCountry <- TRUE  } else {  UseCountry <- weather$countrynum%in%countrynum  }
  if(is.null(states)) {  UseState <- TRUE  } else {  UseState <- weather$statenum%in%states  }
  UseWeather <- UseYear & UseCountry & UseState
  
  if(is.null(routes)) routes=GetRoutes(Dir)
  if(is.null(countrynum)) {  UseCountry <- TRUE  } else {  UseCountry <- routes$countrynum%in%countrynum  }
  if(is.null(states)) {  UseState <- TRUE  } else {  UseState <- routes$statenum%in%states  }
  UseRoutes <- UseCountry & UseState
  
  CommonNames <- names(Data)[names(Data)%in%names(weather)]
  CommonNames <- CommonNames[CommonNames%in%names(routes)]
  
  # Subset data
  # First, sites sampled in chosen year(s)
  weather=subset(weather, subset=UseWeather, 
                 select=c(CommonNames, "Year", "Month", "Day", "RunType"))
  # Route data for sites sampled in chosen years
  routes=subset(routes, subset=UseRoutes & routes$routeID%in%weather$routeID, select=c(CommonNames, "Latitude", "Longitude"))
  
  AllData <- merge(Data, weather, all=TRUE) # by=c("routeID", "RPID"), 
  AllData <- merge(AllData, routes, all=TRUE) # by="routeID", 
  AllData$SumCount <- apply(AllData[,grep("^count", names(AllData))],1,sum, na.rm=TRUE)
  if(!Zeroes) AllData <- subset(AllData, AllData$SumCount>0)
  AllData
}
