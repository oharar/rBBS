context("Get50RouteData")

test_that("Get50RouteData works correctly", {
#  skip_on_cran()
  dat <- Get50RouteData(countrynum=NULL, states=c(89, 40:45), AOU=c(4050, 3850), year=2010, 
                                weather=NULL, routes=NULL, Zeroes=FALSE)
  
  expect_is(dat, "data.frame")
  expect_equal(ncol(dat), 66)
  expect_equal(length(grep("^Stop", names(dat))), 50)
})
