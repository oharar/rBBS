\name{Get10RouteData}
\alias{Get10RouteData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Get10RouteData(AOU, spcodes, weather = NULL, routes = NULL, year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{AOU}{
%%     ~~Describe \code{AOU} here~~
}
  \item{spcodes}{
%%     ~~Describe \code{spcodes} here~~
}
  \item{weather}{
%%     ~~Describe \code{weather} here~~
}
  \item{routes}{
%%     ~~Describe \code{routes} here~~
}
  \item{year}{
%%     ~~Describe \code{year} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (AOU, spcodes, weather = NULL, routes = NULL, year) 
{
    file = spcodes$FileString[spcodes$AOU == AOU]
    dat = GetUnzip(ZipName = paste("ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/", 
        file, ".zip", sep = ""), FileName = paste(file, ".csv", 
        sep = ""))
    dat$routeID = paste(dat$statenum, dat$Route)
    if (is.null(routes)) 
        routes = GetUnzip(ZipName = "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Routes.zip", 
            FileName = "routes.csv")
    if (is.null(routes$routeID)) 
        routes$routeID = paste(routes$statenum, routes$Route)
    if (is.null(weather)) 
        weather = GetUnzip(ZipName = "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Weather.zip", 
            FileName = "weather.csv")
    if (is.null(weather$routeID)) 
        weather$routeID = paste(weather$statenum, weather$Route)
    weather = subset(weather, subset = weather$Year \%in\% year, 
        select = c("countrynum", "statenum", "Year", "Route", 
            "RPID", "TotalSpp", "RunType", "routeID"))
    routes = subset(routes, subset = routes$routeID \%in\% weather$routeID, 
        select = c("countrynum", "statenum", "Route", "Lati", 
            "Longi", "routeID"))
    dat = subset(dat, subset = dat$Year \%in\% year & dat$Aou == 
        AOU, select = c("countrynum", "statenum", "Year", "Route", 
        "count10", "count20", "count30", "count40", "count50", 
        "routeID"))
    AllData = merge(dat, weather, all = TRUE)
    AllData = merge(AllData, routes, all = TRUE)
    AllData$SumCount = apply(AllData[, grep("count[1-5]0", names(AllData))], 
        1, sum, na.rm = TRUE)
    AllData
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
