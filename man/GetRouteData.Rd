\name{GetRouteData}
\alias{GetRouteData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to query 10 or 50 stop data for species, year, and states
}
\description{
This function imports the 10 or 50 stop species data from either the USGS BBS ftp server or another repository
}
\usage{
GetRouteData(AOU = NULL, countrynum = NULL, states = NULL, year,
                weather = NULL, routes = NULL, Zeroes = TRUE, TenStops = TRUE, 
                Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/")
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{AOU}{
Vector of species' AOU code
}
  \item{countrynum}{
Vector of country codes, either 124 (Canada), 484 (Mexico), or 840 (USA).
}
  \item{states}{
Vector of state codes. If they are unique to one country, countrynum can be NULL.
}
  \item{year}{
Year(s) for which data is wanted: must be after 1997 (pre-1997 data not included yet)
}
  \item{weather}{
Data frame with Weather data. Can be NULL, then function will extract the data
}
  \item{routes}{
Data frame with routes data. Can be NULL, then function will extract the data
}
  \item{Zeroes}{
Logical: if TRUE (the default) will return all routes sampled in relevant years, otherwise only the routes where the species was observed.
}
  \item{TenStops}{
Logical: if TRUE (the default) get 10-stop data. If false, get 50-stop data.
}
  \item{Dir}{
Directory where the data will be extracted from. Defaults to USGS server at ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/
}
}
\details{
 The pre-1997 data from Canada is not included.
}
\value{
Data frame with the following columns for all data:
\item{countrynum }{The three digit identification code for country.  See RegionCodes.txt file for key.}
\item{statenum }{The two digit numerical code that identifies the state, province or territory where the route was run.  See RegionCodes.txt file for key.}
\item{routeID }{character code for route. Should be unique. made of paste of state & route IDs}
\item{route }{The three digit code that identifies the route - unique within states.}
\item{rpid }{Three digit run protocol identification number.  See RunProtocolID.txt for key.}
\item{aou }{The five digit species identification code.}

\item{Year }{The year}
\item{Month}{The month}
\item{Day}{The day}
\item{RunType}{If this run is acceptable by BBS standards, then this column is 1, otherwise it is 0.}
\item{Latitude}{The latitude of the route start point in decimal degrees.}
\item{Longitude}{The longitude of the route start point in decimal degrees.}
\item{SumCount}{Total number of individuals of that species counted}

For 10 stop data, these columns in addition:
\item{count10 }{Total individuals of the species recorded on stops 1-10.}
\item{count20 }{Total individuals of the species recorded on stops 11-20.}
\item{count30 }{Total individuals of the species recorded on stops 21-30.}
\item{count40 }{Total individuals of the species recorded on stops 31-40.}
\item{count50 }{Total individuals of the species recorded on stops 41-50.}
\item{stoptotal }{Total number of stops out of 50 on which the species was recorded.}
\item{speciestotal }{The total number of species recorded on that run of the route.}

For 10 stop data, these columns in addition:
\item{stop1 ... stop50}{Total individuals of the species recorded on that stop. }
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J. Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent Wildlife Research Center, Laurel, MD
}
\author{
Bob O'Hara
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{GetWeather}} gets the weather for the routes
}
\examples{
## Get data for pileated woodpecker & roadrunner for some states
 Data <- GetRouteData(AOU=c(4050, 3850), countrynum=NULL, states=c(89, 40:45), year=2010, 
                      TenStops = TRUE, Zeroes=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
