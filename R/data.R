#' Population, Prices, and Quantities for 60 countries (1980).
#'
#' A dataset with prices and quantities for 11 goods in 60 countries 1980.
#' Prices and expenditure were collected around the world by the International
#' Comparison Project, and this is the rescaled dataset such that all US prices
#' are set to unity, and quantities are measured as expenditues in USD.
#'
#' @format A data frame with 60 rows and 24 variables:
#' \describe{
#' \item{country}{Name of country (chr)}
#' \item{pop1980}{1980 population in country (in thousands) }
#' \item{p1}{Price of food}
#' \item{p2}{Price of beverages}
#' \item{p3}{Price of tobacco}
#' \item{p4}{Price of clothing and footwear}
#' \item{p5}{Price of gross rents}
#' \item{p6}{Price of fuel and power}
#' \item{p7}{Price of house furnishings}
#' \item{p8}{Price of medical care}
#' \item{p9}{Price of transport and communication}
#' \item{p10}{Price of recreation and education}
#' \item{p11}{Price of miscellaneous}
#' \item{q1}{Quantity of food (per capita)}
#' \item{q2}{Quantity of beverages (per capita)}
#' \item{q3}{Quantity of tobacco (per capita)}
#' \item{q4}{Quantity of clothing and footwear (per capita)}
#' \item{q5}{Quantity of gross rents (per capita)}
#' \item{q6}{Quantity of house furnishings (per capita)}
#' \item{q7}{Quantity of house furnishings (per capita)}
#' \item{q8}{Quantity of medical care (per capita)}
#' \item{q9}{Quantity of transport and communications (per capita)}
#' \item{q10}{Quantity of recreating and education (per capita)}
#' \item{q11}{Quantity of miscellaneous (per capita)}
#' }
#' @source From the paper Neary, J Peter. 2004. "Rationalizing the
#' Penn World Table: True Multilateral Indices for International Comparisons
#' of Real Income." American Economic Review, 94 (5): 1411-1428.
#' \url{https://www.aeaweb.org/articles?id=10.1257/0002828043052286}
#' Data collected at \url{http://users.ox.ac.uk/~econ0211/gaia/gaia.htm}
"Neary2004"
