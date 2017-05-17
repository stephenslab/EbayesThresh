context("EbayesThresh")

# ----------------------------------------------------------------------
test_that("beta.laplace recovers result from EbayesThresh v1.3.2 when s=1",{
  expect_equal(beta.laplace(c(-2,1,0,-4,5),s = 1),
               c(0.889852029651,-0.380041716606,-0.561817771773,
                 285.459466672351,15639.884914542939),
               tolerance = 1e-6)
})
