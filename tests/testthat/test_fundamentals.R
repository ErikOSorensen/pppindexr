context("Fundamentals")
m <- 3
n <- 2

P <- matrix(runif(n*m),ncol=n)
Q <- matrix(runif(n*m),ncol=n)


test_that("Conforming budget shares", {
  expect_identical(nrow(budget_shares(P,Q)), nrow(Q))
  expect_identical(ncol(budget_shares(P,Q)), ncol(Q))
})

test_that("Homogeneous prices indices", {
  expect_equal(p_laspeyre(P[1,], P[2,],Q[1,]), p_laspeyre(P[1,], P[2,],10*Q[1,]))
  expect_equal(p_laspeyre(10*P[1,], P[2,], Q[1,]), 10*p_laspeyre(P[1,],P[2,],Q[1,]))
  expect_equal(p_paasche(P[1,], P[2,],Q[1,]), p_paasche(P[1,], P[2,],10*Q[1,]))
  expect_equal(p_paasche(10*P[1,], P[2,], Q[1,]), 10*p_paasche(P[1,],P[2,],Q[1,]))
})

test_that("Homogeneous quantity indices", {
  expect_equal(10*q_laspeyre(P[1,],Q[1,], Q[2,]), q_laspeyre(P[1,],10*Q[1,], Q[2,]))
  expect_equal(10*q_paasche(P[1,],Q[1,], Q[2,]), q_paasche(P[1,],10*Q[1,], Q[2,]))
  expect_equal(q_laspeyre(P[1,],Q[1,], Q[2,]), q_laspeyre(10*P[1,],Q[1,], Q[2,]))
  expect_equal( q_paasche(P[1,],Q[1,], Q[2,]),  q_paasche(10*P[1,],Q[1,], Q[2,]))
})

test_that("Inverse symmetry of country pair comparisons", {
  expect_equal(q_fisher(P,Q)[1,m], 1/q_fisher(P,Q)[m,1])
  expect_equal(q_tornqvist(P,Q)[1,m], 1/q_tornqvist(P,Q)[m,1])
})
