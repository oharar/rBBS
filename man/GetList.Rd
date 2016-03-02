\name{GetList}
\alias{GetList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read in list of species names from a file 
}
\description{
Read in list of species names from a file 
}
\usage{
GetList(str, 
Dir = "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/", 
sleep = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{str}{
string for file name, see ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Species/ 
}
  \item{Dir}{
dirctory, defaults to USGS server: "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/"
}
  \item{sleep}{
length of time to sleep (max. 10), so that the ftp server gets a chance to breathe. Can be NULL
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
Data frame with these columns:
\item{Seq }{phylogenetic sequence number}
\item{AOU }{American Ornithological Union code number}
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
## Get woodpeckers and friends
## Species <- GetList("TrogWoodp")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
