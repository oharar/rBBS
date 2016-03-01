
# Get Route data (i.e. routes metadata)
GetRoutes <- function() {
  routes=GetUnzip(ZipName="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Routes.zip", FileName="routes.csv")
  routes$routeID=paste(routes$statenum, routes$Route)
  routes
}
