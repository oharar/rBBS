# Functions to extract BBS data from their ftp archive
# library(RCurl)


####################
# Read BBS meta-data
####################
# Read in list of species names, from SpeciesList.txt, and then extract list of where the data is kept
# Outputs data frame with these columns:
#   Seq: phylogenetic sequence number
#   AOU: Amer. Ornithological Union code number
#   English_Common_Name: English Common Name
#   French_Common_Name: French Common Name
#   Spanish_Common_Name: Spanish Common Name
#   ORDER: Taxonomic order
#   Family: Taxonomic family
#   Genus: Taxonomic genus
#   Species: Taxonomic species name

GetSpNames <- function() {
  SpCols=c(unlist(read.table("ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/SpeciesList.txt", skip=5, nrows=1, stringsAsFactors=F)))
  # read in species metadata: need to use a connection to pass the encoding correctly 
  #    (thanks to Peter Dalgaard & Brian Ripley for help with this)
  con <- file("ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/SpeciesList.txt",encoding="Latin1")
  SpCodes=read.fwf(con, widths=1+nchar(SpCols), skip=6, header=F, stringsAsFactors=F, strip.white=T)
  # read column names
  names(SpCodes)=c(unlist(read.table("ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/SpeciesList.txt", skip=4, nrows=1, stringsAsFactors=F)))
  SpCodes
}

####################
# Read in list of species names from a file 
# Arguments:
#   str: string for file name, see ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/ 
#   dir: dirctory with files. Defaults to "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/SpeciesListsForGroups/"
#   sleep: length of time to sleep (max. 10), so that the ftp server gets a chance to breathe. Can be NULL
# Outputs data frame with these columns:
#   Seq: phylogenetic sequence number
#   AOU: Amer. Ornithological Union code number
#   EnglishFull: English Common Name
#   FrenchName: French Common Name
#   FileString: String that gives file name stem for 10 stop data

GetList=function(str, dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/SpeciesListsForGroups/", sleep=NULL) {
  File=paste(dir, str,"Species.csv", sep="")
  FileCols=c(unlist(read.table(File, skip=0, nrows=2, stringsAsFactors=F))) # read column names & widths
  con <- file(File,encoding="Latin1")
  FileCodes=read.fwf(con, widths=1+nchar(FileCols[grep("-", FileCols)]), skip=2, header=F, stringsAsFactors=F, strip.white=T)

  # read column names
  names(FileCodes)=FileCols[!grepl("-", FileCols)]
  FileCodes$FileString=str # Set name of file
# Sleep
  if(!is.null(sleep)) {
    if(!is.numeric(sleep)) stop("sleep should be a number or null")
    if(sleep>10) {
      message("sleep >10, so reset to 10")
      sleep <- 10
    }
    Sys.sleep(sleep)
  }
  FileCodes
}


# Get all species names, as well as name of file where the 10-stop data are contained
# 
# Arguments:
#   dir: dirctory with files. Defaults to "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/SpeciesListsForGroups/"
#   sleep: length of time to sleep (max. 10), so that the ftp server gets a chance to breathe. Defaults to NULL
# Outputs data frame with these columns:
#   Seq: phylogenetic sequence number
#   AOU: Amer. Ornithological Union code number
#   EnglishFull: English Common Name
#   FrenchName: French Common Name
#   FileString: String that gives file name stem for 10 stop data

GetAllSpNames <- function(dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/SpeciesListsForGroups/", sleep=NULL) {
  require(RCurl)
  # Read in names of which species are in which files in SpeciesListsForGroups
  # Get names of species list files
  FileString=getURL(dir, dirlistonly = TRUE)
  Files=strsplit(FileString, "\n")[[1]]
  Files=gsub("Species.csv","",Files[grep(".csv", Files)])
  
# This is unstable, hence splitting it into 4. Should work out how to do error handling properly
  Tables.1=sapply(Files[1:5], GetList, dir=dir, sleep=1, simplify=FALSE)
  Tables.2=sapply(Files[6:10], GetList, dir=dir, sleep=1, simplify=FALSE)
  Tables.3=sapply(Files[11:15], GetList, dir=dir, sleep=1, simplify=FALSE)
  Tables.4=sapply(Files[16:19], GetList, dir=dir, sleep=1, simplify=FALSE)
  
  Table=rbind(do.call(rbind, Tables.1), do.call(rbind, Tables.2), do.call(rbind, Tables.3), do.call(rbind, Tables.4))
  
 Table  
}




# Function to extract csv data from a zipped archive
#  Code adapted from http://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
#  Arguments:
#    ZipName: name of zipped archive
#    FileName: name of file in zipped archive
# Output:
#    A data frame (or whatever else read.csv wants to give)
GetUnzip=function(ZipName, FileName) {
  temp <- tempfile()
  download.file(ZipName,temp)
  data <- read.csv(unz(temp, FileName))
  unlink(temp)
  data
}

# Function to query 10 stop data for a species in a year
#  Arguments:
#  AOU - species' AOU code
#  spcodes: data frame with columns AOU and FileString
#  weather: Data frame with Weather data. Can be NULL, then function will extract the data
#  routes: Data frame with reather data. Can be NULL, then function will extract the data
#  year: Year(s) for which data is wanted
# Output:
#   Data frame with the following columns:
#     countrynum  The three digit identification code for country.  See RegionCodes.txt file for key.
#     statenum 	The two digit numerical code that identifies the state, province or territory where the route was run.  See RegionCodes.txt file for key.
#     Route       	The three digit code that identifies the route - unique within states.
#     routeID:      character code for route. Should be unique. made of paste of state & route IDs
#     Year        	The year
#     AOU         	The five digit species identification code.
#     count10     	Total individuals of the species recorded on stops 1-10.
#     count20     	Total individuals of the species recorded on stops 11-20.
#     count30     	Total individuals of the species recorded on stops 21-30.
#     count40     	Total individuals of the species recorded on stops 31-40.
#     count50     	Total individuals of the species recorded on stops 41-50.
#     RPID  	      Three digit run protocol identification number.  See RunProtocolID.txt for key.
#   TotalSpp    The total number of species recorded on that run of the route.
#   RunType     If this run is acceptable by BBS standards, then this column is 1, otherwise it is 0.
#   Lati        The latitude of the route start point in decimal degrees.
#   Longi     	The longitude of the route start point in decimal degrees.
#   SumCount  Total number of individuals counted

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
