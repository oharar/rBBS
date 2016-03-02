# Get Route data (i.e. routes metadata)
#' @export GetRoutes
GetRoutes <- function(Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/") {
  routes=GetUnzip(ZipName=paste0(Dir, "Routes.zip"), FileName="routes.csv")
  routes$routeID=paste(routes$statenum, routes$Route)
  routes
}
