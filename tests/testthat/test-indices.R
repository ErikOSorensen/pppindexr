m <- 3
n <- 2
P <- matrix(runif(n*m),ncol=n)
Q <- matrix(runif(n*m),ncol=n)
Pneg <- P
Pneg[1,2] = -0.1
Qneg <- Q
Qneg[2,2] = -1
Plarge = matrix(runif((n+1)*m), nrow=m)
Qlarge = matrix(runif(n*(m+1)), ncol=n)


test_that("GK validates input", {
  expect_error(gk(Plarge,Q))
  expect_error(gk(Pneg,Q))
  expect_error(gk(P,Qlarge))
  expect_error(gk(P,Qneg))
  expect_error(gk(P,Q, pop=c(1,2)))

})

test_that("EKS validates input",{
  expect_error(eks(Plarge,Q))
  expect_error(eks(Pneg,Q))
  expect_error(eks(P,Qlarge))
  expect_error(eks(P,Qneg))
})

test_that("CCD validates input",{
  expect_error(ccd(Plarge,Q))
  expect_error(ccd(Pneg,Q))
  expect_error(ccd(P,Qlarge))
  expect_error(ccd(P,Qneg))
})


test_that("GK output conforms", {
  expect_type(gk(P,Q), "list")
  expect_vector(gk(P,Q)$p, size=n)
  expect_vector(gk(P,Q)$y, size=m)
  expect_vector(ygk(P,Q), size=m)
  expect_vector(ygk(P,Q, min_scale=TRUE), size=m)
  expect_vector(pgk(P,Q), size=n)
  expect_equal(min(ygk(P,Q, min_scale=TRUE)), 1)
})

test_that("EKS output conforms", {
  expect_type(eks(P,Q), "double")
  expect_vector(eks(P,Q), size=m)
  expect_equal(min(eks(P,Q)), 1)
})

test_that("CCD output conforms", {
  expect_type(ccd(P,Q), "double")
  expect_vector(ccd(P,Q), size=m)
  expect_equal(min(ccd(P,Q)), 1)
})

test_that("GK homogeneous", {
  expect_equal(ygk(P,Q), ygk(10*P,Q))
  expect_equal(pgk(P,Q), pgk(P, 10*Q))
  expect_equal(10*ygk(P,Q), ygk(P,10*Q))
})

test_that("EKS homogeneous", {
  expect_equal(eks(P,Q), eks(10*P,Q))
  expect_equal(eks(P,Q), eks(P,10*Q))
})

test_that("CCD homogeneous", {
  expect_equal(ccd(P,Q), ccd(10*P,Q))
  expect_equal(ccd(P,Q), ccd(P,10*Q))
})
