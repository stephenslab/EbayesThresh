test_that("wpost.laplace is stable for large a and small s",{
  biga = 1e6
  smalls = 1e-6
  expect_equal(wpost.laplace(0.1,1,1,biga),0.1,tol=1e-3)
  expect_equal(wpost.laplace(0.1,1,smalls,1),1,tol=1e-3)
})

test_that("pmeancond and pmean2cond returns 0 for large a",{
  biga = 1e6
  expect_equal(pmeancond(1,1,biga),0,tol=1e-3)
  expect_equal(pmeancond(-1,1,biga),0,tol=1e-3)
  expect_equal(pmean2cond(1,1,biga),0,tol=1e-3)
  expect_equal(pmean2cond(-1,1,biga),0,tol=1e-3)
})
