
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pppindexr

Price and Quantity Indices for International Comparisons: The EKS, CCD,
and the Geary-Khamis real income indices.

Basic functionality tests are implemented, with some reasonable unit
tests.

## Installation

You can install the development version of `pppindexr` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ErikOSorensen/pppindexr")
```

## Example

Let us look at a 3x2 example (three countries of unequal size, two
goods) and define some arbitrary prices and quantities (one row per
country):

``` r
library(pppindexr)
ex1 <- data.frame(
  population = c(1, 2, 3),
  p1 = c(1, 2, 1),
  p2 = c(1, 1, 3),
  q1 = c(1, 1, 1),
  q2 = c(3, 1, 2)
)
# CCD, EKS,  and Gheary Khamis indices:
ex1$eks = eks(ex1[, 2:3], ex1[, 4:5])
ex1$ccd = ccd(ex1[, 2:3], ex1[, 4:5])
ex1$gk  = ygk(ex1[, 2:3], ex1[, 4:5], 
              pop = ex1$population, min_scale=TRUE)
ex1 |> knitr::kable(digits=3)
```

| population |  p1 |  p2 |  q1 |  q2 |   eks |   ccd |    gk |
|-----------:|----:|----:|----:|----:|------:|------:|------:|
|          1 |   1 |   1 |   1 |   3 | 1.915 | 1.902 | 2.151 |
|          2 |   2 |   1 |   1 |   1 | 1.000 | 1.000 | 1.000 |
|          3 |   1 |   3 |   1 |   2 | 1.456 | 1.440 | 1.576 |

The quantity indices are scaled with respect to the “country” with the
lowest real income.
