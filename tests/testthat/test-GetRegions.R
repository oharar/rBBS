context("GetRegions")

test_that("GetRegions works correctly", {
#  skip_on_cran()
  Reg <- GetRegions()

  expect_is(Reg, "data.frame")
  expect_is(Reg$CountryName, "character")
  expect_is(Reg$FileName, "character")
  expect_equal(grepl("\\.zip", Reg$FileName), !is.na(Reg$FileName))
  
  expect_equal(ncol(Reg), 5)
})
