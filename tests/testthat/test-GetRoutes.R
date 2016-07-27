context("GetRoutes")

test_that("GetRoutes works correctly", {
#  skip_on_cran()
  route <- GetRoutes()

  expect_is(route, "data.frame")
  expect_is(route$routeID, "character")
  expect_equal(ncol(route), 13)
})
