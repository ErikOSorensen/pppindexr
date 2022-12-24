#' Paasche price index.
#'
#' The Paasche price index is calculated as
#'  \deqn{ \frac{\sum_i p^t_i q^t_t}{\sum_i p^b_i q^t_i}.}
#' Arguments can either be vectors or matrices.
#'
#' @param pt A n-vector (or m x n matrix) of prices in current period.
#' @param pb A n-vector (or m x n matrix) of prices in the baseline period.
#' @param qt A n-vector (or n x n matrix) of quantities in the current period.
#' @return Price index between current an baseline period (number or vector).
#' @export
p_paasche <- function(pt, pb, qt) {
  assertthat::are_equal(length(pt), length(pb))
  assertthat::are_equal(length(qt), length(pb))

  if (is.matrix(pt)) {
    r <- rowSums(pt * qt) / rowSums(pb * qt)
  }
  else {
    r <- sum(pt * qt) / sum(pb * qt)
  }
  r
}

#' Paasche quantity index.
#'
#' @param pt A n-vector (or m x n matrix) of prices in current period.
#' @param qt A n-vector (or m x n matrix) of quantities in the current period.
#' @param qb A n-vector (or m x n matrix) of quantities in the baseline period.
#' @return Quantity index between current an baseline period (number or vector).
#' @export
q_paasche <- function(pt, qt, qb) {
  p_paasche(qt, qb, pt)
}

#' Laspeyre price index.
#'
#' The Laspeyre price index calculated as
#' \deqn{ \frac{\sum_i p^t_i q^b_t}{\sum_i p^b_i q^b_i}.}
#' Arguments can either be vectors or matrices.
#'
#' @param pt A n-vector (or m x n matrix) of prices in current period.
#' @param pb A n-vector (or m x n matrix) of prices in the baseline period.
#' @param qb A n-vector (or m x n matrix) of quantities in the baseline period.
#' @return Price index between current an baseline period (number or vector).
#' @export
p_laspeyre <- function(pt, pb, qb) {
  assertthat::are_equal(length(pt), length(pb))
  assertthat::are_equal(length(qb), length(pt))

  if (is.matrix(pt)) {
    r <- rowSums(pt * qb) / rowSums(pb * qb)
  } else {
    r <- sum(pt * qb) / sum(pb * qb)
  }
  r
}

#' Laspeyre quantity index.
#'
#' The Laspeyre quantity index is calculated as
#' \deqn{ \frac{\sum_i q^t_i p^b_t}{\sum_i q^b_i p^b_i}.}
#' Arguments can either be vectors or matrices.
#'
#' @param pb A n-vector (or m x n matrix) of prices in the baseline period.
#' @param qt A n-vector (or m x n matrix) of quantities in the current period.
#' @param qb A n-vector (or m x n matrix) of quantities in the baseline period.
#' @return Price index between current an baseline period (number or vector).
#' @export
q_laspeyre <- function(pb, qt, qb) {
  p_laspeyre( qt, qb, pb)
}







#' Fisher ideal price index.
#'
#' The Fisher ideal index takes the geometric mean of the
#' Paasche and the Laspeyre indices.
#' If there are m countries and n goods, the data are (m x n) matrices.
#'
#' @param P An  m x n  matrix of prices (row country, column goods).
#' @param Q An m x n matrix of quantities (row country, column goods).
#' @return An m x m matrix of country pair comparisons.
#' @export
p_fisher <- function(P, Q) {
  assertthat::are_equal(nrow(P), nrow(Q))
  assertthat::are_equal(ncol(P), ncol(Q))
  m <- nrow(P)
  n <- ncol(P)
  r <- matrix( data=NA, nrow=m, ncol=m)
  for (i in 1:m) {
    for (j in 1:m) {
      r[i,j] <- sqrt(p_laspeyre(P[i,], P[j,], Q[i,]) *
                       p_paasche(P[i,], P[j,], Q[j,]))
    }
  }
  r
}

#' Fisher ideal quantity index.
#'
#' The Fisher ideal index takes the geometric mean of the
#' Paasche and the Laspeyre indices.
#' If there are m countries and n goods, the data are (m x n) matrices.
#'
#' @param P An m x n matrix of prices (row country, column goods).
#' @param Q An m x n matrix of quantities (row country, column goods).
#' @return an m x m matrix of country pair comparisons.
#' @export
q_fisher <- function(P, Q) {
  p_fisher(Q,P)
}

#' Törnqvist quantity index.
#'
#' The Törnqvist quantity index is an alternative to the Fisher ideal index.
#'
#' @param P An m x n matrix of prices (row country, column goods).
#' @param Q An m x n matrix of quantities (row country, column goods).
#' @return an m x m matrix of country pair comparisons.
#' @export
q_tornqvist <- function(P, Q) {
  assertthat::are_equal(nrow(P), nrow(Q))
  assertthat::are_equal(ncol(P), ncol(Q))
  PQ <- P * Q
  logQ <- log(Q)
  W <- budget_shares(P,Q)
  m <- nrow(P)
  n <- ncol(P)
  r <- matrix( data=0, nrow=m, ncol=m)
  for (i in 1:m) {
    for (j in 1:m) {
      for (k in 1:n) {
        r[i,j] <- r[i,j] + 0.5 * (W[i,k] + W[j,k]) * (logQ[i,k] - logQ[j,k])
      }
    }
  }
  exp(r)
}

#' Budget shares
#'
#' Returns a matrix of budget shares corresponding to prices and quantities.
#'
#' @param P An m x n matrix of prices (row country, column goods).
#' @param Q An m x n matrix of quantities (row country, column goods).
#' @keywords internal
#' @return An m x n matrix of budget shares
budget_shares <- function(P, Q) {
  assertthat::are_equal(nrow(P), nrow(Q))
  assertthat::are_equal(ncol(P), ncol(Q))
  PQ <- P * Q
  PQ / rowSums(PQ)
}
