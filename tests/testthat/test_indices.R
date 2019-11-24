context("Indices")

m <- 3
n <- 2

P <- matrix(runif(n*m),ncol=n)
Q <- matrix(runif(n*m),ncol=n)

test_that("GK output conforms", {
  expect_is(gk(P,Q), "list")
  expect_vector(gk(P,Q)$p, size=n)
  expect_vector(gk(P,Q)$y, size=m)
})

test_that("EKS output conforms", {
  expect_is(eks(P,Q), "numeric")
  expect_vector(eks(P,Q), size=m)
})

test_that("CCD output conforms", {
  expect_is(ccd(P,Q), "numeric")
  expect_vector(ccd(P,Q), size=m)
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
