test_that("method of moments complains correctly if called with universal thresh",{
  expect_error(ebayesthresh(rnorm(100),opt_method="mm"))
  expect_error(ebayesthresh(rnorm(100),universalthresh=FALSE,opt_method="mm"))
  expect_equal(ebayesthresh(rnorm(100),universalthresh=FALSE,a=NA,opt_method="mm",verbose=TRUE)$opt_method,"mm")
})

