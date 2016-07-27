# Read in list of species names, from SpeciesList.txt, and then extract list of where the data is kept
#' @export GetSpNames
GetSpNames <- function(Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/") {
  File <- paste0(Dir,"SpeciesList.txt")
  spnames.all <- scan(File, sep="\n", what=character(), blank.lines.skip = FALSE, fileEncoding="Latin1")
  SpCols <- spnames.all[-(1:grep("^-", spnames.all))]
  
  Widths <- c(1,1+cumsum(nchar(strsplit(spnames.all[8], ' ')[[1]])))
  
  SpData <- as.data.frame(sapply(2:length(Widths), function(wh, wid, dat) {
    trimws(substr(dat, wid[wh-1]+1, wid[wh]))
  }, wid=Widths, dat=SpCols), stringsAsFactors = FALSE)
  names(SpData) <- strsplit(spnames.all[7], '[ ]{2,}')[[1]]
  SpData$Seq <- as.numeric(SpData$Seq)
  SpData$AOU <- as.numeric(SpData$AOU)
  SpData
}



