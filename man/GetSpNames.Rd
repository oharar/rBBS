\name{GetSpNames}
\alias{GetSpNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read BBS meta-data
}
\description{
Read in list of species names, from SpeciesList.txt, and then extract list of where the data is kept
}
\usage{
GetSpNames()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Data frame with these columns:
\item{Seq }{phylogenetic sequence number}
\item{AOU }{American Ornithological Union code number}
\item{English_Common_Name }{English Common Name}
\item{French_Common_Name }{French Common Name}
\item{Spanish_Common_Name }{Spanish Common Name}
\item{ORDER }{Taxonomic order}
\item{Family }{Taxonomic family}
\item{Genus }{Taxonomic genus}
\item{Species }{Taxonomic species name}

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
function () 
{
    SpCols = c(unlist(read.table("ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/SpeciesList.txt", 
        skip = 5, nrows = 1, stringsAsFactors = F)))
    con <- file("ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/SpeciesList.txt", 
        encoding = "Latin1")
    SpCodes = read.fwf(con, widths = 1 + nchar(SpCols), skip = 6, 
        header = F, stringsAsFactors = F, strip.white = T)
    names(SpCodes) = c(unlist(read.table("ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/SpeciesList.txt", 
        skip = 4, nrows = 1, stringsAsFactors = F)))
    SpCodes
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
