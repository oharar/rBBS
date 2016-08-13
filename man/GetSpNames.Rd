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
GetSpNames(Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/")
}
\arguments{
  \item{Dir}{
Directory to get data. Defaults to ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/, the USGS FTP server
}
}
%- maybe also 'usage' for other objects documented here.
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
\examples{
## Here you go...
## Species <- GetSpNames()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
