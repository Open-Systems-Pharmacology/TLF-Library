% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-timeprofile.R
\name{plotTimeProfile}
\alias{plotTimeProfile}
\title{plotTimeProfile}
\usage{
plotTimeProfile(
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
Producing Time Profile plots
}
\examples{
# Produce a Time profile plot with observed and simulated data
obsData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))
simTime <- seq(1, 10, 0.1)
simData <- data.frame(
  x = simTime,
  y = 10 * exp(-simTime),
  ymin = 8 * exp(-simTime),
  ymax = 12 * exp(-simTime)
)

plotTimeProfile(
  data = simData,
  observedData = obsData,
  dataMapping = TimeProfileDataMapping$new(x = "x", y = "y", ymin = "ymin", ymax = "ymax"),
  observedDataMapping = ObservedDataMapping$new(x = "x", y = "y")
)
}
\seealso{
Other molecule plots: 
\code{\link{plotBoxWhisker}()},
\code{\link{plotCumulativeTimeProfile}()},
\code{\link{plotDDIRatio}()},
\code{\link{plotGrid}()},
\code{\link{plotHistogram}()},
\code{\link{plotObsVsPred}()},
\code{\link{plotObservedTimeProfile}()},
\code{\link{plotPKRatio}()},
\code{\link{plotPieChart}()},
\code{\link{plotQQ}()},
\code{\link{plotResVsPred}()},
\code{\link{plotResVsTime}()},
\code{\link{plotSimulatedTimeProfile}()},
\code{\link{plotTornado}()}
}
\concept{molecule plots}
