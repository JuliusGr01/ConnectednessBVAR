% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VarianceTest.R
\name{VarianceTest}
\alias{VarianceTest}
\title{Variance Test}
\usage{
VarianceTest(
  formula,
  data,
  alpha = 0.05,
  method = c("Bartlett", "Brown-Forsythe", "Fligner-Killeen", "Fisher", "Levene"),
  na.rm = TRUE
)
}
\arguments{
\item{formula}{a formula of the form lhs ~ rhs where lhs gives the sample values and rhs the corresponding groups.}

\item{data}{a tibble or data frame containing the variables in the formula formula}

\item{alpha}{the level of significance to assess variance homogeneity. Default is set to alpha = 0.05.}

\item{method}{a character string to select one of the variance homogeneity tests: "Bartlett", "Brown-Forsythe", "Fisher" and "Fligner-Killeen".}

\item{na.rm}{Ha logical value indicating whether NA values should be stripped before the computation proceeds.}
}
\value{
Get bivariate portfolio weights
}
\description{
VarianceTest performs variance homogeneity tests including Ftest, Bartlett, Brown-Forsythe and Fligner-Killeen tests.
}
\references{
Antonakakis, N., Cunado, J., Filis, G., Gabauer, D., & de Gracia, F. P. (2020). Oil and asset classes implied volatilities: Investment strategies and hedging effectiveness. Energy Economics, 91, 104762.
}
\author{
David Gabauer
}
