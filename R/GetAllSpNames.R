# Get all species names, as well as name of file where the 10-stop data are contained
#' @export GetAllSpNames
GetAllSpNames <- function(dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/SpeciesListsForGroups/", sleep=NULL) {
  # Read in names of which species are in which files in SpeciesListsForGroups
  # Get names of species list files
  FileString=RCurl::getURL(dir, dirlistonly = TRUE)
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
