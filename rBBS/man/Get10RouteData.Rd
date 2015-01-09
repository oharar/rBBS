\name{Get10RouteData}
\alias{Get10RouteData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to query 10 stop data for a species in a year
}
\description{
This function imports the 10 stop species data from the USGS BBS ftp server
}
\usage{
Get10RouteData(AOU, spcodes, weather = NULL, routes = NULL, year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{AOU}{
species' AOU code
}
  \item{spcodes}{
data frame with columns AOU and FileString
}
  \item{weather}{
Data frame with Weather data. Can be NULL, then function will extract the data
}
  \item{routes}{
Data frame with reather data. Can be NULL, then function will extract the data
}
  \item{year}{
Year(s) for which data is wanted
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
Data frame with the following columns:
\item{countrynum }{The three digit identification code for country.  See RegionCodes.txt file for key.}
\item{statenum }{The two digit numerical code that identifies the state, province or territory where the route was run.  See RegionCodes.txt file for key.}
\item{Route }{The three digit code that identifies the route - unique within states.}
\item{routeID }{character code for route. Should be unique. made of paste of state & route IDs}
\item{Year }{The year}
\item{AOU }{The five digit species identification code.}
\item{count10 }{Total individuals of the species recorded on stops 1-10.}
\item{count20 }{Total individuals of the species recorded on stops 11-20.}
\item{count30 }{Total individuals of the species recorded on stops 21-30.}
\item{count40 }{Total individuals of the species recorded on stops 31-40.}
\item{count50 }{Total individuals of the species recorded on stops 41-50.}
\item{RPID }{Three digit run protocol identification number.  See RunProtocolID.txt for key.}
\item{TotalSpp }{The total number of species recorded on that run of the route.}
\item{RunType }{If this run is acceptable by BBS standards, then this column is 1, otherwise it is 0.}
\item{Lati }{The latitude of the route start point in decimal degrees.}
\item{Longi }{The longitude of the route start point in decimal degrees.}
\item{SumCount }{Total number of individuals counted}
}
\references{
Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J. Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent Wildlife Research Center, Laurel, MD
}
\author{
Bob O'Hara
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
