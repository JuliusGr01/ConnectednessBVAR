% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TimeConnectedness.R
\name{TimeConnectedness}
\alias{TimeConnectedness}
\title{Diebold and Yilmaz (2009, 2012) connectedness approach}
\usage{
TimeConnectedness(
  Phi = NULL,
  Sigma = NULL,
  nfore = 10,
  generalized = TRUE,
  corrected = FALSE,
  FEVD = NULL
)
}
\arguments{
\item{Phi}{VAR coefficient matrix}

\item{Sigma}{Residual variance-covariance matrix}

\item{nfore}{H-step ahead forecast horizon}

\item{generalized}{Orthorgonalized/generalized FEVD}

\item{corrected}{Boolean value whether corrected or standard TCI should be computed}

\item{FEVD}{Alternatively, to provide Phi and Sigma it is also possible to use FEVD directly.}
}
\value{
Get connectedness measures
}
\description{
This function allows to calculate the Diebold and Yilmaz (2009, 2012) connectedness measures.
}
\examples{
\donttest{
#Replication of DY2012
data("dy2012")
fit = VAR(dy2012, configuration=list(nlag=4))
dca = TimeConnectedness(Phi=fit$B, Sigma=fit$Q, nfore=10, generalized=TRUE)
dca$TABLE
}
}
\references{
Diebold, F. X., & Yilmaz, K. (2009). Measuring financial asset return and volatility spillovers, with application to global equity markets. The Economic Journal, 119(534), 158-171.

Diebold, F. X., & Yilmaz, K. (2012). Better to give than to receive: Predictive directional measurement of volatility spillovers. International Journal of Forecasting, 28(1), 57-66.
}
\author{
David Gabauer
}
