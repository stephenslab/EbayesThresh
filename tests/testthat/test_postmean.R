test_that("postmean gives same result computed two different ways",{
  x <- c(-2,1,0,-4,5)
  s <- c(1,2,3,2,1)
  a <- 0.2
  w <- 0.7
  expect_equal(postmean.laplace(x, s, w, a),wpost.laplace(w,x,s,a)*pmeancond(x,s,a),tolerance = 1e-6)
})