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
Get10RouteData(AOU, spcodes, weather = NULL, routes = NULL, year, vars=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{AOU}{
species' AOU code
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
  \item{vars}{
Variables to be kept. Defaults to NULL; when all data are kept.
  \item{Zeroes}{
Bernoulli: if TRUE (the default) will return all routes sampled in relevant years, otherwise only the routes where the species was observed.
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

data(SpCodes)
library(MASS)

# Import Collared Dove data

ColDoveAOU <- SpCodes$AOU[grep("Eurasian Collared", SpCodes$EnglishFull)]
CollaredDove <- Get10RouteData(ColDoveAOU, weather = NULL, routes = NULL, year=c(1995,2000,2005, 2010))

routes <- GetRoutes()
CollaredDove$Longi <- sapply(CollaredDove$routeID, function(id, Rt) Rt$Longi[Rt$routeID==id], Rt=routes)
CollaredDove$Lati <- sapply(CollaredDove$routeID, function(id, Rt) Rt$Lati[Rt$routeID==id], Rt=routes)

# Admire the spread of the Eurasian Collared Dove this millenium
par(mfrow=c(2,2), mar=c(1,1,3,1))
for(yr in unique(CollaredDove$Year)) {
  eqscplot(CollaredDove$Longi[CollaredDove$Year==yr], CollaredDove$Lati[CollaredDove$Year==yr], xlim=c(-140,-50), ylim=c(24,65), cex=0.3,
       pch=15, col=c("darkgreen", "red")[(CollaredDove$SumCount[CollaredDove$Year==yr]>0)+1],
       ann=FALSE, axes=FALSE)
    title(main=yr)  
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
