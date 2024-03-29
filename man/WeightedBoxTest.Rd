% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WeightedBoxTest.R
\name{WeightedBoxTest}
\alias{WeightedBoxTest}
\title{WeightedBoxTest}
\usage{
WeightedBoxTest(
  x,
  lag = 1,
  type = c("Box-Pierce", "Ljung-Box", "Monti"),
  fitdf = 0,
  sqrd.res = FALSE,
  log.sqrd.res = FALSE,
  abs.res = FALSE,
  weighted = TRUE
)
}
\arguments{
\item{x}{a numeric vector or univariate time series, or residuals of a fitted time series}

\item{lag}{the statistic will be based on lag autocorrelation coefficients. lag=1 by default}

\item{type}{test to be performed, partial matching is used. "Box-Pierce" by default}

\item{fitdf}{number of degrees of freedom to be subtracted if x is a series of residuals, set at 0 by default}

\item{sqrd.res}{A flag, should the series/residuals be squared to detect for nonlinear effects?, FALSE by default}

\item{log.sqrd.res}{A flag, should a log of the squared series/residuals be used to detect for nonlinear effects? FALSE by default}

\item{abs.res}{A flag, should the absolute series or residuals be used to detect for nonlinear effects? FALSE by default}

\item{weighted}{A flag determining if the weighting scheme should be utilized. TRUE by default. If set to FALSE, the traditional test is performed with no weights}
}
\value{
Get Uninformative Prior
}
\description{
Weighted portmanteau tests for testing the null hypothesis of adequate ARMA fit and/or for detecting nonlinear processes. Written in the style of Box.test() and is capable of performing the traditional Box Pierce (1970), Ljung Box (1978) or Monti (1994) tests.
}
\references{
Box, G. E. P. and Pierce, D. A. (1970), Distribution of residual correlations in autoregressive-integrated moving average time series models. Journal of the American Statistical Association, 65, 1509-1526.

Fisher, T. J. and Gallagher, C. M. (2012), New Weighted Portmanteau Statistics for Time Series Goodness-of-Fit Testing. Journal of the American Statistical Association, accepted.

Ljung, G. M. and Box, G. E. P. (1978), On a measure of lack of fit in time series models. Biometrika 65, 297-303.

Mahdi, E. and McLeod, A. I. (2012), Improved multivariate portmanteau test. Journal of Time Series Analysis 65(2), 297-303.

Monti, A. C. (1994), A proposal for a residual autocorrelation test in linear models. Biometrika 81(4), 776-780.

Pena, D. and Rodriguez, J. (2002) A powerful portmanteau test of lack of fit for time series. Journal of the American Statistical Association 97(458), 601-610.
}
\author{
David Gabauer
}
