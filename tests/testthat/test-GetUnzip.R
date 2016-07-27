context("GetUnzip")

test_that("GetUnzip works correctly", {
#  skip_on_cran()
  dat <- GetUnzip(ZipName="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/States/Alabama.zip", 
                  FileName="Alabama.csv")
  
  expect_is(dat, "data.frame")
  expect_equal(ncol(dat), 13)
})



