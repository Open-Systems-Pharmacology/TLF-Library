% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-molecule-plots.R
\name{.addLLOQLayer}
\alias{.addLLOQLayer}
\title{.addLLOQLayer}
\usage{
.addLLOQLayer(plotObject, data, mapLabels, direction)
}
\arguments{
\item{plotObject}{A \code{ggplot} object}

\item{data}{A data.frame with labels mapped to properties and obtained from a \code{DataMapping} object}

\item{mapLabels}{List of mapped label names passed to \code{ggplot2::aes_string}}

\item{direction}{Whether to draw LLOQ lines for x (vertical), y (horizontal) or x and y (both).}
}
\value{
A \code{ggplot} object
}
\description{
Add a line layer representing the Lower Limit Of Quantification (LLOQ)
}
\keyword{internal}
