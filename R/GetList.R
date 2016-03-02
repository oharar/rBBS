# Read in list of species names from a file 
#' @export GetList
GetList=function(str, Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/", sleep=NULL) {
  File=paste0(Dir, "Species/SpeciesListsForGroups/", str,"Species.csv")
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
