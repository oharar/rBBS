# Code to import list of species names, and link them to file names so they can be used in other functions

dir <- "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/SpeciesListsForGroups/"
library(RCurl)
library(devtools)

# Read in names of which species are in which files in SpeciesListsForGroups
# Get names of species list files
FileString <- getURL(dir, dirlistonly = TRUE)
Files <-strsplit(FileString, "\n")[[1]]
Files <-gsub("Species.csv","",Files[grep(".csv", Files)])
  
# This is unstable, hence splitting it into 4. Should work out how to do error handling properly
Tables.1 <- sapply(Files[1:5], GetList, dir=dir, sleep=1, simplify=FALSE)
Tables.2 <- sapply(Files[6:10], GetList, dir=dir, sleep=1, simplify=FALSE)
Tables.3 <- sapply(Files[11:15], GetList, dir=dir, sleep=1, simplify=FALSE)
Tables.4 <- sapply(Files[16:19], GetList, dir=dir, sleep=1, simplify=FALSE)
  
SpCodes <- rbind(do.call(rbind, Tables.1), do.call(rbind, Tables.2), do.call(rbind, Tables.3), do.call(rbind, Tables.4))

use_data(SpCodes, overwrite=TRUE)
