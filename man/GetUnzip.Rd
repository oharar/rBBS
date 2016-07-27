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
## Download woodpecker data from BBS
Rep <- "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/"
dat=GetUnzip(ZipName=paste0(Rep, "States/Florida.zip"), 
               FileName="Florida.csv")

\dontrun{
## Download from a local repository
##   Use your own LocalRepos
  LocalRepos <- "~/Dropbox/"
  dat=GetUnzip(ZipName=paste0(LocalRepos, 
               "BBS/DataFiles/Species/TrogWoodp.zip"), 
               FileName="TrogWoodp.csv")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
