# Read in list of regions (State/Prov/TerrName), from RegionCodes.txt, and then extract list of where the 10-stop data is kept
#' @export GetRegions
GetRegions <- function(Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/", ZipFiles = FALSE) {
  File <- paste0(Dir,"RegionCodes.txt")
  CountryWidths <- c(unlist(read.table(File, skip=3, nrows=1, stringsAsFactors=F)))
  # read in country metadata: need to use a connection to pass the encoding correctly 
  #    (thanks to Peter Dalgaard & Brian Ripley for help with this)
  con <- file(File,encoding="Latin1")
  CountryCodes <- read.fwf(con, widths=1+nchar(CountryWidths), skip=4, n=3, header=F, stringsAsFactors=F, strip.white=T) 
  # read column names
  names(CountryCodes) <- c(unlist(read.table(File, skip=2, nrows=1, stringsAsFactors=F)))

# Read in state/province/terratory names and code
  RegionWidths <- c(unlist(read.table(File, skip=10, nrows=1, stringsAsFactors=F)))
  con <- file(File,encoding="Latin1")
  RegionCodes <- read.fwf(con, widths=1+nchar(RegionWidths), skip=11, header=F, stringsAsFactors=F, strip.white=T) 
  # read column names
  names(RegionCodes) <- c(unlist(read.table(File, skip=9, nrows=1, stringsAsFactors=F)))
  RegionCodes$CountryName <- sapply(RegionCodes$countrynum, function(num, CCode) {
    CCode$CountryName[num==CCode$CountryNum]
  }, CCode = CountryCodes)
  
# Get zip file names
  if(ZipFiles) {
    readme.all <- scan(paste0(Dir, "README.txt"), sep="\n", what=character(), blank.lines.skip = FALSE, fileEncoding="Latin1")
    readme.all <- gsub("\t","",readme.all)
    PrecedingLine <- grep("States Directory:", readme.all)
    EndLine <- which(readme.all[PrecedingLine:length(readme.all)]=="")[1]
    
    ZipFiles.tmp <- strsplit(readme.all[PrecedingLine+(2:(EndLine-2))], '[ ]{2,}')
    ZipFiles <- data.frame(State = unlist(lapply(ZipFiles.tmp, function(x) x[3])), 
                           File = unlist(lapply(ZipFiles.tmp, function(x) x[1])), stringsAsFactors = FALSE)
    
    RegionCodes$FileName10stop <- sapply(RegionCodes$`State/Prov/TerrName`, function(Name, zipf) {
      #    Name <- "Newfoundland and Labrador"
      file <- zipf$File[tolower(zipf$State)==tolower(Name)]
      if(length(file)==0) {
        file <- zipf$File[sapply(tolower(zipf$State), function(state) any(grepl(paste0("^",state), tolower(Name))))]
      }
      if(length(file)==0) file <- NA
      file
    }, zipf=ZipFiles)
    
    Files50stop <- sapply(1:10, function(ind) {
      zipf <- paste0(Dir, "/50-StopData/1997ToPresent_SurveyWide/Fifty", ind, ".zip")
      dat <- GetUnzip(zipf, paste0("fifty", ind, ".csv"))
      res <- unique(dat[,c("countrynum", "statenum")])
      res$File50stop <- as.character(paste0("fifty", ind, ".csv"))
      res
    }, simplify=FALSE)
    Files50stop.df <- ldply(Files50stop)
    
    RegionCodes$FileName50stop <- apply(RegionCodes, 1, function(rc, Fnames) {
      cn <- as.integer(unlist(rc["countrynum"]))
      rcd <- as.integer(unlist(rc["RegionCode"]))
      File <- Fnames$File50stop[Fnames$countrynum==cn & Fnames$statenum==rcd]
      if(length(File)>1) warning("state in more than one file, returning first state")
      if(length(File)==0) File <- NA
      File
    }, Fnames=Files50stop.df)
    RegionCodes$FileName50stop <- gsub("^f", "F", RegionCodes$FileName50stop)
    RegionCodes$FileName50stop <- gsub("csv$", "zip", RegionCodes$FileName50stop)
  }
  
  RegionCodes
}
