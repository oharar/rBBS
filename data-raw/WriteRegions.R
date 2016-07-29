RegionsForZipFiles <- GetRegions(ZipFiles = TRUE)
devtools::use_data(RegionsForZipFiles, overwrite=TRUE, internal = TRUE)
