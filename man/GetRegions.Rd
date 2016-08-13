\name{GetRegions}
\alias{GetRegions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read BBS meta-data
}
\description{
Read in list of states/provinces/territories, and also names of zip files where the 10 stop data is kept.
}
\usage{
GetRegions(Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/", ZipFiles = FALSE)
}
\arguments{
  \item{Dir}{
Directory to get data. Defaults to ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/, the USGS FTP server
}
  \item{ZipFiles}{
Logical: should the names of the zip files for teh 10- and 50-stop data be added? Defaults to FALSE.
}
}
%- maybe also 'usage' for other objects documented here.
\details{
This is meta-data collated from the full database. Nore that not all regions have a zip file: in particular, there is no Mexican data, or data from Puerto Rico or Washington D.C.
}
\value{
%%  ~Describe the value returned
Data frame with these columns:
\item{countrynum}{Country code}
\item{RegionCode }{Region code}
\item{State/Prov/TerrName}{Name of (US) state, (Canadian) province or (Mexican) territory}
\item{CountryName}{Name of country}
\item{FileName10stop}{Name of zip file with 10 stop data}
\item{FileName50stop}{Name of zip file with 50 stop data}
}
\references{
Sauer, J. R., J. E. Hines, J. E. Fallon, K. L. Pardieck, D. J. Ziolkowski, Jr., and W. A. Link. 2014. The North American Breeding Bird Survey, Results and Analysis 1966 - 2012. Version 02.19.2014 USGS Patuxent Wildlife Research Center, Laurel, MD
}
\author{
Bob O'Hara
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
## Here you go...
## Regions <- GetRegions()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
