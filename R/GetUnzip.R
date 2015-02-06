# Function to extract csv data from a zipped archive
GetUnzip=function(ZipName, FileName) {
  temp <- tempfile()
  download.file(ZipName,temp)
  data <- read.csv(unz(temp, FileName))
  unlink(temp)
  data
}
