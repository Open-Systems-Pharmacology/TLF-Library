% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-axis.R
\name{getSqrtTickLabels}
\alias{getSqrtTickLabels}
\title{getSqrtTickLabels}
\usage{
getSqrtTickLabels(ticks)
}
\arguments{
\item{ticks}{numeric values of the ticks}
}
\value{
Expressions to use in \code{ticklabels} input parameter of \code{setXAxis} and \code{setYAxis} functions
}
\description{
Get ticklabels expressions for sqrt scale plots
}
\examples{
ticks <- sqrt(c(1, 5, 10, 50, 100, 500))
getSqrtTickLabels(ticks)
}
