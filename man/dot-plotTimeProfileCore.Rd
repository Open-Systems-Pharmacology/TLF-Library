% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-timeprofile.R
\name{.plotTimeProfileCore}
\alias{.plotTimeProfileCore}
\title{.plotTimeProfileCore}
\usage{
.plotTimeProfileCore(
  data = NULL,
  metaData = NULL,
  dataMapping = NULL,
  observedData = NULL,
  observedDataMapping = NULL,
  plotConfiguration = NULL,
  plotObject = NULL
)
}
\arguments{
\item{data}{A data.frame to use for plot.}

\item{metaData}{A named list of information about \code{data} such as the \code{dimension} and \code{unit} of its variables.}

\item{dataMapping}{A \code{TimeProfileDataMapping} object mapping \code{x}, \code{y}, \code{ymin}, \code{ymax} and aesthetic groups to their variable names of \code{data}.}

\item{observedData}{A data.frame to use for plot.
Unlike \code{data}, meant for simulated data, plotted as lines and ribbons;
\code{observedData} is plotted as scatter points and errorbars.}

\item{observedDataMapping}{An \code{ObservedDataMapping} object mapping \code{x}, \code{y}, \code{ymin}, \code{ymax} and aesthetic groups to their variable names of \code{observedData}.}

\item{plotConfiguration}{An optional \code{TimeProfilePlotConfiguration} object defining labels, grid, background and watermark.}

\item{plotObject}{An optional \code{ggplot} object on which to add the plot layer}
}
\value{
A \code{ggplot} object
}
\description{
Producing Core of Time Profile plots
}
\keyword{internal}
