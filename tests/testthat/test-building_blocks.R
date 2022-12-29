m <- 3
n <- 2
P <- matrix(runif(n * m), ncol = n)
Q <- matrix(runif(n * m), ncol = n)


test_that("Homogeneous prices indices", {
  expect_equal(p_laspeyre(P[1, ], P[2, ], Q[1, ]), p_laspeyre(P[1, ], P[2, ], 10 *
                                                                Q[1, ]))
  expect_equal(p_laspeyre(10 * P[1, ], P[2, ], Q[1, ]), 10 * p_laspeyre(P[1, ], P[2, ], Q[1, ]))
  expect_equal(p_paasche(P[1, ], P[2, ], Q[1, ]), p_paasche(P[1, ], P[2, ], 10 *
                                                              Q[1, ]))
  expect_equal(p_paasche(10 * P[1, ], P[2, ], Q[1, ]), 10 * p_paasche(P[1, ], P[2, ], Q[1, ]))
})

test_that("Homogeneous quantity indices", {
  expect_equal(10 * q_laspeyre(P[1, ], Q[1, ], Q[2, ]), q_laspeyre(P[1, ], 10 *
                                                                     Q[1, ], Q[2, ]))
  expect_equal(10 * q_paasche(P[1, ], Q[1, ], Q[2, ]), q_paasche(P[1, ], 10 *
                                                                   Q[1, ], Q[2, ]))
  expect_equal(q_laspeyre(P[1, ], Q[1, ], Q[2, ]), q_laspeyre(10 * P[1, ], Q[1, ], Q[2, ]))
  expect_equal(q_paasche(P[1, ], Q[1, ], Q[2, ]),  q_paasche(10 * P[1, ], Q[1, ], Q[2, ]))
})

test_that("Symmetry of country pair comparisons", {
  expect_equal(q_fisher(P, Q), 1./t(q_fisher(P,Q)))
  expect_equal(q_tornqvist(P, Q), 1 /t(q_tornqvist(P, Q)))
  expect_equal(diag(q_fisher(P,Q)), rep(1, dim(P)[1]))
  expect_equal(diag(p_fisher(P,Q)), rep(1, dim(P)[1]))
  expect_equal(diag(q_tornqvist(P,Q)), rep(1, dim(P)[1]))
})
