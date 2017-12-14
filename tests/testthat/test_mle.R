test_that("mle is appropriately invariant to multiplying x and s by constant",{
  set.seed(23)
  s = rgamma(100,1,1)
  x = rnorm(100,0,s)
  x[1:50] = x[1:50] + rexp(50,2)
  e1 = ebayesthresh(x,sdev=s,universalthresh=FALSE,a=NA,threshrule = "mean")
  e2 = ebayesthresh(2*x,sdev=2*s,universalthresh=FALSE,a=NA,threshrule = "mean")
  expect_equal(e1/e2,rep(0.5,100),tol=1e-2)
})