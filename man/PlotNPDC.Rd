% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotNPDC.R
\name{PlotNPDC}
\alias{PlotNPDC}
\title{Dynamic net pairwise connectedness plot}
\usage{
PlotNPDC(
  dca,
  ca = NULL,
  path = NULL,
  ylim = c(NULL, NULL),
  selection = NULL,
  width = 10,
  height = 7,
  ...
)
}
\arguments{
\item{dca}{Connectedness object}

\item{ca}{Compare dca object with a single connectedness object or a list of of connectedness objects}

\item{path}{Path where plots should be saved}

\item{ylim}{A vector including the lower and upper limit of the y-axis}

\item{selection}{Indicator of the illustrated series}

\item{width}{The width of the graphics region in inches}

\item{height}{The height of the graphics region in inches}

\item{...}{Arguments to be passed to methods, such as graphical parameters (see par).}
}
\value{
Return connectedness plot
}
\description{
Visualize dynamic net pairwise connectedness
}
