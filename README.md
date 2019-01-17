
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/oharar/rBBS.svg?branch=master)](https://travis-ci.org/oharar/rBBS)

rBBS
====

R package to import USGS' BBS data into R

Installation
------------

At the moment, the package is only available on GitHub, so can be installed like this:

``` r
devtools::install_github("oharar/rBBS")
library("rBBS")
```

Example
-------

The package is intended to extract abundance/incidence data from the BBS data. In practice, it is worth downloading a local copy from the ftp server, as this will speed up reading the data.

We can start by searching for the speciew we are intersted in. There is a GetSpNames() function to get a data table of species IDs (including French and Spanish names). We can look for what Americans call the English Sparrow. It is listed as the House Sparrow (which is what the English Sparrow is called in England).

``` r
library(rBBS)

RegionMetaData <- GetRegions()
RoutesMetaData <- GetRoutes()
WeatherMetaData <- GetWeather()

Species <- GetSpNames()
Species[grep("^House", Species$English_Common_Name),
        c("AOU", "English_Common_Name", "Genus", "Species")]
#>     AOU English_Common_Name       Genus    Species
#> 519 721          House Wren Troglodytes      aedon
#> 730 519         House Finch  Haemorhous  mexicanus
#> 745 688       House Sparrow      Passer domesticus
```

We can see that the AOU code is 6882. So now we can search the database for the species (840 is the country number for USA, 1980 is just a convenient date to use):

``` r
HouseSparrowInUSA <- GetRouteData(AOU=6882, countrynum = 840, year = 1980, 
                                   weather = WeatherMetaData, routes = RoutesMetaData)
```

Using Zeroes=TRUE means that we include locations that were surveyed but where the species was not observed. We can thus plot where the house (sorry, English) sparrow was observed on a map:

``` r
library(maps)
map('state')
points(HouseSparrowInUSA$Longitude[is.na(HouseSparrowInUSA$stoptotal)], 
       HouseSparrowInUSA$Latitude[is.na(HouseSparrowInUSA$stoptotal)], 
       cex=0.3, pch=16, col="hotpink")
points(HouseSparrowInUSA$Longitude[!is.na(HouseSparrowInUSA$stoptotal)], 
       HouseSparrowInUSA$Latitude[!is.na(HouseSparrowInUSA$stoptotal)], 
       cex=0.4, pch=16, col="blue")
legend(-120, 30, c("Absent", "Observed"), pch=15, col=c("hotpink", "blue"))
```

![](README-unnamed-chunk-5-1.png)

So we can see that the species was present throughout the Lower 48 states of the US, except possibly Nevada (which is probably because only 2 routes were sampled). We can look at this in more detail ,and see that indeed there was a lack of sampling in the early 1980s in Nevada, but with more routes, the house sparrow was seen:

``` r
NevadaCode <- RegionMetaData$RegionCode[RegionMetaData$`State/Prov/TerrName` == "NEVADA"]
NevadaYears <- 1970:2015
HSInNevada <- GetRouteData(AOU = 6882, countrynum = 840, 
                                     states = NevadaCode, year = NevadaYears, Zeroes = TRUE)

NevadaSumm <- data.frame(Year = NevadaYears, 
                        Routes = tapply(HSInNevada$stoptotal, list(HSInNevada$Year), length),
                        NObs = tapply(HSInNevada$stoptotal, list(HSInNevada$Year), 
                                      function(dat) sum(!is.na(dat))))

par(mar=c(4.1,4.1,1,1))
plot(NevadaSumm$Year, NevadaSumm$Routes, type="l", col="hotpink", ylim=c(0,max(NevadaSumm$Routes)), 
     yaxt="n", xlab="Year", ylab="Number of routes")
lines(NevadaSumm$Year, NevadaSumm$NObs, type="l", col="blue")
legend(1992, 90, c("Total", "Routes with house sparrows"), 
       lty=1, col=c("hotpink", "blue"))
axis(2, las=1)
```

![](README-unnamed-chunk-6-1.png)

Contributions
-------------

All contributions are welcome. Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

To Do
-----

-   write code to import migrant/non-breeder data
-   write code to import noise data
-   check route & weather data are useable in their own right
-   document SpCodes
-   sort out non-ASCII characters Once that's done, improve functionality, stability etc.
