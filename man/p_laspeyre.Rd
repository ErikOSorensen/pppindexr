% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/building_blocks.R
\name{p_laspeyre}
\alias{p_laspeyre}
\title{Laspeyre price index.}
\usage{
p_laspeyre(pt, pb, qb)
}
\arguments{
\item{pt}{A n-vector (or m x n matrix) of prices in current period.}

\item{pb}{A n-vector (or m x n matrix) of prices in the baseline period.}

\item{qb}{A n-vector (or m x n matrix) of quantities in the baseline period.}
}
\value{
Price index between current an baseline period (number or vector).
}
\description{
The Laspeyre price index calculated as
\deqn{ \frac{\sum_i p^t_i q^b_t}{\sum_i p^b_i q^b_i}.}
Arguments can either be vectors or matrices.
}
\examples{
Pt <- matrix(1:6, ncol = 2)
Pb <- Pt*0.7
Qb <- matrix(2:7, ncol = 2)
p_laspeyre(Pt, Pb, Qb)
p_laspeyre(Pt[1,], Pb[1,], Qb[1,])
}
