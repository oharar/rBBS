context("GetRouteData")

test_that("GetRouteData works correctly for 10 routes", {
#  skip_on_cran()
  dat10 <- GetRouteData(AOU=c(4050, 3850), countrynum=NULL, states=c(89, 40:45), 
                        year=2010, TenStops = TRUE, 
                        weather=NULL, routes=NULL, Zeroes=FALSE)
  
  expect_is(dat10, "data.frame")
  expect_equal(ncol(dat10), 20)
  expect_equal(nrow(dat10), 111)
  expect_equal(length(grep("^count[0-9]", names(dat10))), 5)
})

test_that("GetRouteData works correctly for 50 routes", {
  #  skip_on_cran()
  dat50 <- GetRouteData(AOU=c(4050, 3850), countrynum=NULL, states=c(89, 40:45), 
                        year=2010, TenStops = FALSE, 
                        weather=NULL, routes=NULL, Zeroes=FALSE)
  
  expect_is(dat50, "data.frame")
  expect_equal(ncol(dat50), 63)
  expect_equal(nrow(dat50), 375)
  expect_equal(length(grep("^stop", names(dat50))), 50)
})
