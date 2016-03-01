\name{GetWeather}
\alias{GetWeather}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read in weather and other meta-data
}
\description{
Reads Weather meta-data in BBS, from Weather.zip. Contains other meta-data too, so is useful elsewhere
}
\usage{
GetWeather(Dir=NULL) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Dir}{
string for file name, defaults to ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/ if NULL.
  }
}
\details{
Documented here: ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/WeatherInf.txt
}
\value{
Data frame with these columns:
\item{CountryNum}{Three digit code that identifies country: 124 (Canada), 484 (Mexico), or 840 (USA)}
\item{StateNumber}{Two digit numerical code that identifies the state, province, or territory}
\item{Route}{Three digit code that identifies the route.}
 \item{RPID}{The run protocol identification number.}
\item{Year}{The year.}
\item{Month}{The month the route was surveyed (1-12).}
\item{Day}{The day the route was surveyed (1-31).}
\item{ObsN}{Unique observer identification number.}
\item{TotalSpp}{The total number of species recorded on that run of the route.}
\item{StartTemp}{The temperature recorded at the beginning of the run of the route.}
\item{EndTemp}{The temperature recorded at the end of the run of the route.}
\item{TempScale}{The scale in which the temperatures were recorded. F stands for Fahrenheit; 
	     C stands for Celcius.}
\item{StartWind}{The Beaufort wind speed code recorded at the beginning of the run of the route.}
\item{EndWind}{The Beaufort wind speed code recorded at the end of the run of the route.}
\item{StartSky}{The Weather Bureau sky code recorded at the beginning of the run of the route.}
\item{EndSky}{The Weather Bureau sky code recorded at the end of the run of the route.}
\item{StartTime}{The time the run of the route began, recorded in 24 hour local time.}
\item{EndTime}{The time the run of the route ended, recorded in 24 hour local time.}
\item{Assistant}{Did someone assist? 1 if they did, otherwise 0.}
\item{RunType}{If this run is acceptable by BBS standards, then 1, otherwise 0.}
\item{ routeID }{unique route ID}
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
## GetWeather()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
