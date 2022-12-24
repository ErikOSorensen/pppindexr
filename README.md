
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pppindexr

Price and Quantity Indices for International Comparisons: The EKS, CCD,
and the Geary-Khamis real income indices.

Rudimentary functionality and some unit tests are implemented. \##
Installation

You can install the development version of pppindexr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ErikOSorensen/pppindexr")
```

## Example

Let us look at a 2x2 example (two countries of equal size, two goods)
and define some arbitrary prices and quantities (one row per country):

``` r
library(pppindexr)
ex1 <- data.frame(population=c(1,1),
                  p1=c(1,2), p2=c(1,1), 
                  q1=c(1,1), q2=c(2,2))
# EKS index and Gheary Khamis:
ex1$eks = eks(ex1[,2:3],ex1[,4:5])
ex1$gk  = gk( ex1[,2:3], ex1[,4:5], pop=ex1$pop)
ex1
#>   population p1 p2 q1 q2 eks                 gk
#> 1          1  1  1  1  2   1 1.428571, 1.000000
#> 2          1  2  1  1  2   1 3.428571, 3.428571
```