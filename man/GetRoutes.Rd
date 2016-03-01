\name{GetRoutes}
\alias{GetRoutes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get meta-data about BBS routes
}
\description{
Gets meta-data about BBS routes
}
\usage{
GetRoutes(Dir=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Dir}{
string for file name, defaults to ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/ if NULL.
}
}
\details{
See ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/RouteInf.txt for documentation.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Data frame with these columns:
\item{countrynum}{Integer code for country: 124 (Canada), 484 (Mexico), or 840 (USA)}
\item{statenum}{Integer code for state}
\item{Route}{Integer code for route}
\item{Active}{Integer code for whether route is active: 1 if active (note: might be active, but not sampled)}
\item{Lati}{Latitude}
\item{Longi}{Longitude}
\item{Stratum}{The BBS physiographic stratum code for that route. }
\item{BCR}{Bird Conservation Region}
\item{LandTypeID}{Integer for type of land that a route is located on}
\item{RouteTypeID}{Route substrate; 1 = Roadside, 2 = Water, 3 = Off-road}
\item{RouteTypeDetailId}{Indicates route length and selection criteria}
\item{routeID}{Route ID (mainly for internal use, to make sure routes are unique)}
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
## Get woodpeckers and friends
## Routes <- GetRoutes()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
