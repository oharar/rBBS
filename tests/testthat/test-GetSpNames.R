context("GetSpNames")

test_that("GetSpNames works correctly", {
#  skip_on_cran()
  SpNames <- GetSpNames()

  expect_is(SpNames, "data.frame")
  expect_is(SpNames$Seq, "numeric")
  expect_is(SpNames$AOU, "numeric")
  expect_is(SpNames$Genus, "character")
  expect_equal(ncol(SpNames), 9)
})
