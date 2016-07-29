context("GetRegions")

test_that("GetRegions works correctly", {
#  skip_on_cran()
  Reg <- GetRegions(ZipFiles = TRUE)
  ZipFiles <- paste0("Fifty",1:10,".zip")

  expect_is(Reg, "data.frame")
  expect_is(Reg$CountryName, "character")
  expect_is(Reg$FileName10stop, "character")
  expect_is(Reg$FileName50stop, "character")
  expect_equal(grepl("\\.zip", Reg$FileName10stop), !is.na(Reg$FileName10stop))
  expect_equal(grepl("\\.zip", Reg$FileName50stop), !is.na(Reg$FileName50stop))
  expect_equal(all(ZipFiles%in%Reg$FileName50stop), TRUE)
})
