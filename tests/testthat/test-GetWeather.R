context("GetWeather")

test_that("GetWeather works correctly", {
#  skip_on_cran()
  weather <- GetWeather()

  expect_is(weather, "data.frame")
  expect_is(weather$routeID, "character")
  expect_equal(ncol(weather), 22)
})
