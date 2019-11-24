#' EKS quantity index
#'
#' @param P An m x n matrix of prices (row country, column goods).
#' @param Q An m x n matrix of quantities (row country, column goods).
#' @return An m-vector of income (relative to minimum income).
#' @export
eks <- function(P, Q) {
  assertthat::are_equal(nrow(P), nrow(Q))
  assertthat::are_equal(ncol(P), ncol(Q))
  m <- nrow(P)

  log_fisher <- log( q_fisher(P,Q))
  log_eks <- rowSums(log_fisher)/m
  r <- exp(log_eks - min(log_eks))
  r
}

#' CCD (Caves, Christensen, and Diewert) index
#'
#' @param P An m x n matrix of prices (row country, column goods).
#' @param Q An m x n matrix of quantities (row country, column goods).
#' @return An m-vector of income (relative to minimum income).
#' @export
ccd <- function(P, Q) {
  assertthat::are_equal(nrow(P), nrow(Q))
  assertthat::are_equal(ncol(P), ncol(Q))
  m <- nrow(P)

  log_tornqvist <- log(q_tornqvist(P, Q))
  log_ccd <- rowSums(log_tornqvist)/m
  r <- exp(log_ccd - min(log_ccd))
  r
}
