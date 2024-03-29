% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-boxwhisker.R
\name{.addBoxWhisker}
\alias{.addBoxWhisker}
\title{.addBoxWhisker}
\usage{
.addBoxWhisker(data, metaData, dataMapping, plotConfiguration, plotObject)
}
\arguments{
\item{data}{A data.frame to use for plot.}

\item{metaData}{A named list of information about \code{data} such as the \code{dimension} and \code{unit} of its variables.}

\item{dataMapping}{A \code{BoxWhiskerDataMapping} object mapping \code{x}, \code{y} and aesthetic groups to their variable names of \code{data}.}

\item{plotConfiguration}{An optional \code{BoxWhiskerConfiguration} object defining labels, grid, background and watermark.}

\item{plotObject}{An optional \code{ggplot} object on which to add the plot layer}
}
\value{
A \code{ggplot} object
}
\description{
Add a boxplot layer to a \code{ggplot} object (without outliers)
}
\keyword{internal}
