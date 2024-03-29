% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-axis.R
\name{getPiTickLabels}
\alias{getPiTickLabels}
\title{getPiTickLabels}
\usage{
getPiTickLabels(ticks)
}
\arguments{
\item{ticks}{numeric values of the ticks}
}
\value{
Expressions to use in \code{ticklabels} input parameter of \code{setXAxis} and \code{setYAxis} functions
}
\description{
Get ticklabels expressions for plots with values as ratios of Pi
}
\examples{
ticks <- seq(0, 2 * pi, pi / 2)
getPiTickLabels(ticks)
}
