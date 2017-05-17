context("EbayesThresh")

# ----------------------------------------------------------------------
test_that("beta.laplace recovers result from v1.3.2 package when s=1",{
  x <- c(-2,1,0,-4,5)
  y <- c(+0.889852029651,
         -0.380041716606,
         -0.561817771773,
         +285.459466672351,
         +15639.884914542939)
  expect_equal(beta.laplace(x,s = 1),y,tolerance = 1e-6)
})
