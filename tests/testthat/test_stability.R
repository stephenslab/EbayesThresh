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

test_that("pmeancond and pmean2cond return sensible values for small a",{
  smalla = 1e-6
  expect_equal(pmeancond(10,1,smalla),10,tol=1e-3)
  expect_equal(pmeancond(-10,1,smalla),-10,tol=1e-3)
  expect_equal(pmean2cond(10,1,smalla),101,tol=1e-3)
  expect_equal(pmean2cond(-10,1,smalla),101,tol=1e-3)
})

test_that("posterior median stable for large a",{
  biga=1e6
  expect_equal(postmed.laplace(3,3,0.9,100),0)
  expect_lte(postmed.laplace(3,3,1,biga),postmed.laplace(3,3,1,10))
})

test_that("posterior median and mean scale appropriately",{
  x = 3 
  s=1
  w=0.5
  a=1
  expect_equal(postmed.laplace(x,s,w,a),postmed.laplace(x*100,s*100,w,a/100)/100,tol=1e-6)
  expect_equal(postmean.laplace(x,s,w,a),postmean.laplace(x*100,s*100,w,a/100)/100,tol=1e-6)
  expect_equal(postmean2.laplace(x,s,w,a),postmean2.laplace(x*100,s*100,w,a/100)/100^2,tol=1e-6)
})