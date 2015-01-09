\name{GetUnzip}
\alias{GetUnzip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to extract csv data from a zipped archive
}
\description{
Code adapted from http://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
}
\usage{
GetUnzip(ZipName, FileName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ZipName}{
name of zipped archive
}
  \item{FileName}{
FileName: name of file in zipped archive}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame (or whatever else read.csv wants to give)
}
\references{
http://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
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
function (ZipName, FileName) 
{
    temp <- tempfile()
    download.file(ZipName, temp)
    data <- read.csv(unz(temp, FileName))
    unlink(temp)
    data
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
