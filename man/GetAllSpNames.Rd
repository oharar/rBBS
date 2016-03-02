\name{GetAllSpNames}
\alias{GetAllSpNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get all species names, as well as name of file where the 10-stop data are contained
}
\description{
Get all species names, as well as name of file where the 10-stop data are contained
}
\usage{
GetAllSpNames(dir = 
"ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/SpeciesListsForGroups/",
sleep = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{
dirctory with files. Defaults to "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/SpeciesListsForGroups/"
}
  \item{sleep}{
length of time to sleep (max. 10), so that the ftp server gets a chance to breathe. Defaults to NULL
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Data frame with these columns:
\item{Seq }{phylogenetic sequence number}
\item{AOU }{Amer. Ornithological Union code number}
\item{EnglishFull }{English Common Name}
\item{FrenchName }{French Common Name}
\item{FileString }{String that gives file name stem for 10 stop data}
}
\references{
Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J. Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent Wildlife Research Center, Laurel, MD
}
\author{
Bob O'Hara
}
\note{
Requires RCurl, but is the only function in the package that needs it.

This can be unstable, reading in some files. Thus it would be nice to include the data it imports in the package, and then delete this function. The USGS have been asked about whether this is allowed.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Try this:
## AllNames <- GetAllSpNames()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
