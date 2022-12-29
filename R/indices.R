#' EKS quantity index
#'
#' @param P An m x n matrix (or dataframe) of prices (row country, column goods).
#' @param Q An m x n matrix (or dataframe) of quantities (row country, column goods).
#' @return An m-vector of income (relative to minimum income).
#' @export
#'
#' @examples
#' P <- matrix(1:6, ncol = 2)
#' Q <- matrix(2:7, ncol = n)
#' eks(P, Q)
eks <- function(P, Q) {
  P <- as.matrix(P)
  Q <- as.matrix(Q)
  assertthat::assert_that(all(P >= 0))
  assertthat::assert_that(all(Q >= 0))
  assertthat::assert_that(assertthat::are_equal(dim(P), dim(Q)))
  m <- nrow(P)

  log_fisher <- log(q_fisher(P, Q))
  log_eks <- rowSums(log_fisher) / m
  r <- exp(log_eks - min(log_eks))
  r
}

#' CCD (Caves, Christensen, and Diewert) index
#'
#' @param P An m x n matrix (or dataframe) of prices (row country, column goods).
#' @param Q An m x n matrix (or dataframe) of quantities (row country, column goods).
#' @return An m-vector of income (relative to minimum income).
#' @export
#'
#' @examples
#' P <- matrix(1:6, ncol = 2)
#' Q <- matrix(2:7, ncol = n)
#' ccd(P, Q)
ccd <- function(P, Q) {
  P <- as.matrix(P)
  Q <- as.matrix(Q)
  assertthat::assert_that(all(P >= 0))
  assertthat::assert_that(all(Q >= 0))
  assertthat::assert_that(assertthat::are_equal(dim(P), dim(Q)))
  m <- nrow(P)

  log_tornqvist <- log(q_tornqvist(P, Q))
  log_ccd <- rowSums(log_tornqvist) / m
  r <- exp(log_ccd - min(log_ccd))
  r
}


#' Gheary Khamis world prices and quantity index
#'
#' Calculates the Gheary Khamis "world prices" and the corresponding
#' quantity index (normalized by the prices of the last good being set
#' to unity).
#'
#' For the Gheary Khamis index, it makes a difference whether the quantities
#' are total country-numbers or if they are per capita numbers. If the quantity
#' matrix is per capita, one should also supply a population vector for
#' the proper weighting. If per capita quantities are supplied, the quantity
#' index returned are also per capita.
#'
#' @param P An m x n matrix (or dataframe) of prices (row country, column goods).
#' @param Q An m x n matrix (or dataframe) of quantities (row country, column goods).
#' @param pop Optional m-vector of population numbers (if Q is per capita).
#'
#' @return A list with two elements: pi, an n-vector of prices and y, an m-vector of incomes.
#'
#' @family gk ygk pgk
#' @export
#'
#' @examples
#' P <- matrix(1:6, ncol = 2)
#' Q <- matrix(2:7, ncol = n)
#' gk(P, Q)
#'
gk <- function(P, Q, pop = NULL) {
  P <- as.matrix(P)
  Q <- as.matrix(Q)
  assertthat::assert_that(all(P >= 0))
  assertthat::assert_that(all(Q >= 0))
  assertthat::assert_that(assertthat::are_equal(dim(P), dim(Q)))
  m <- nrow(P) # number of countries
  n <- ncol(P) # number of goods

  if (is.null(pop)) pop <- rep(1, m)
  assertthat::assert_that(assertthat::are_equal(nrow(P), length(pop)))
  Qp <- Q * pop
  W <- budget_shares(P, Qp)
  qhatinv <- diag(1 / colSums(Qp))
  A <- matrix(0, nrow = n + 1, ncol = n)
  b <- rep(0, n + 1)
  A[n + 1, n] <- 1.0
  b[n + 1] <- 1.0
  A[0:n, ] <- qhatinv %*% t(W) %*% Qp - diag(1, nrow = n)
  AA <- t(A) %*% A
  Ab <- t(A) %*% b
  pi <- solve(AA, Ab)
  z <- Q %*% pi
  list(pi = as.vector(pi), y = as.vector(z))
}

#' Geary Khamis quantity index
#'
#' This is a convenience function, which returns only the quantity
#' index vector. Calls \code{\link{gk}} and returns only the index.
#'
#' @param P An m x n matrix (or dataframe) of prices (row country, column goods).
#' @param Q An m x n matrix (or dataframe) of quantities (row country, column goods).
#' @param pop Optional m-vector of population numbers (if Q is per capita).
#' @param min_scale Should real income be scaled in units relative to least income country?
#'
#' @return An m-vector of quantities.
#' @export
#'
#' @examples
#' P <- matrix(1:6, ncol = 2)
#' Q <- matrix(2:7, ncol = n)
#' ygk(P, Q)
#' ygk(P, Q, min_scale=TRUE)
ygk <- function(P, Q, pop = NULL, min_scale=FALSE) {
  gkl <- gk(P, Q, pop = pop)
  if (min_scale) {
    y <- gkl$y / min(gkl$y)
  } else {
    y <- gkl$y
  }
  y
}

#' Geary Khamis world prices
#'
#' This is a convenience function, which returns just the world price
#' vector. Calls \code{\link{gk}} and returns only the prices.
#'
#' @param P An m x n matrix (or dataframe) of prices (row country, column goods).
#' @param Q An m x n matrix (or dataframe) of quantities (row country, column goods).
#' @param pop Optional m-vector of population numbers (if Q is per capita).
#'
#' @return An n-vector of prices.
#' @export
#'
#' @examples
#' P <- matrix(1:6, ncol = 2)
#' Q <- matrix(2:7, ncol = n)
#' pgk(P, Q)
pgk <- function(P, Q, pop = NULL) {
  gk(P, Q, pop = pop)$pi
}
