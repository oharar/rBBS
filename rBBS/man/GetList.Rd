\name{GetList}
\alias{GetList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetList(str, dir = "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/SpeciesListsForGroups/", sleep = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{str}{
%%     ~~Describe \code{str} here~~
}
  \item{dir}{
%%     ~~Describe \code{dir} here~~
}
  \item{sleep}{
%%     ~~Describe \code{sleep} here~~
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
function (str, dir = "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/SpeciesListsForGroups/", 
    sleep = NULL) 
{
    File = paste(dir, str, "Species.csv", sep = "")
    FileCols = c(unlist(read.table(File, skip = 0, nrows = 2, 
        stringsAsFactors = F)))
    con <- file(File, encoding = "Latin1")
    FileCodes = read.fwf(con, widths = 1 + nchar(FileCols[grep("-", 
        FileCols)]), skip = 2, header = F, stringsAsFactors = F, 
        strip.white = T)
    names(FileCodes) = FileCols[!grepl("-", FileCols)]
    FileCodes$FileString = str
    if (!is.null(sleep)) {
        if (!is.numeric(sleep)) 
            stop("sleep should be a number or null")
        if (sleep > 10) {
            message("sleep >10, so reset to 10")
            sleep <- 10
        }
        Sys.sleep(sleep)
    }
    FileCodes
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
