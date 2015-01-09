\name{GetAllSpNames}
\alias{GetAllSpNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get all species names, as well as name of file where the 10-stop data are contained
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetAllSpNames(dir = "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/SpeciesListsForGroups/", sleep = NULL)
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
function (dir = "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/SpeciesListsForGroups/", 
    sleep = NULL) 
{
    require(RCurl)
    FileString = getURL(dir, dirlistonly = TRUE)
    Files = strsplit(FileString, "\n")[[1]]
    Files = gsub("Species.csv", "", Files[grep(".csv", Files)])
    Tables.1 = sapply(Files[1:5], GetList, dir = dir, sleep = 1, 
        simplify = FALSE)
    Tables.2 = sapply(Files[6:10], GetList, dir = dir, sleep = 1, 
        simplify = FALSE)
    Tables.3 = sapply(Files[11:15], GetList, dir = dir, sleep = 1, 
        simplify = FALSE)
    Tables.4 = sapply(Files[16:19], GetList, dir = dir, sleep = 1, 
        simplify = FALSE)
    Table = rbind(do.call(rbind, Tables.1), do.call(rbind, Tables.2), 
        do.call(rbind, Tables.3), do.call(rbind, Tables.4))
    Table
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
