# Read in list of species names, from SpeciesList.txt, and then extract list of where the data is kept
#' @export GetSpNames
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
