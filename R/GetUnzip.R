# Function to extract csv data from a zipped archive
#' @export GetUnzip
GetUnzip=function(ZipName, FileName) {
  if(grepl('^[hf]t+p', ZipName)) {
    temp <- tempfile()
    download.file(ZipName,temp)
    data <- read.csv(unz(temp, FileName))
    unlink(temp)
  } else {
    data <- read.csv(unz(ZipName, FileName))
  }
  data
}
