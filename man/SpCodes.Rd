\name{SpCodes}
\alias{SpCodes}
\docType{data}
\title{
Meta-data for species in BBS
}
\description{
Names of species, AOU codes and which file their data is in
}
\usage{data("SpCodes")}
\format{
  A data frame with 705 observations on the following 5 variables.
  \describe{
    \item{\code{Seq}}{a numeric vector}
    \item{\code{AOU}}{a numeric vector: AOU code}
    \item{\code{EnglishFull}}{a character vector: common English name}
    \item{\code{FrenchName}}{a character vector: common French name}
    \item{\code{FileString}}{a character vector: name of zip file where the data is}
  }
}
\details{
This is meta-data, mainly used to get the correct 10-stop data.
}
\source{
ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/
}
\references{
Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J. Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent Wildlife Research Center, Laurel, MD
}
\examples{
data(SpCodes)
## maybe str(SpCodes) ; plot(SpCodes) ...
}
\keyword{datasets}
