% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-boxwhisker.R
\name{plotBoxWhisker}
\alias{plotBoxWhisker}
\title{plotBoxWhisker}
\usage{
plotBoxWhisker(
  data,
  metaData = NULL,
  outliers = NULL,
  dataMapping = NULL,
  plotConfiguration = NULL,
  plotObject = NULL
)
}
\arguments{
\item{data}{A data.frame to use for plot.}

\item{metaData}{A named list of information about \code{data} such as the \code{dimension} and \code{unit} of its variables.}

\item{outliers}{Logical defining if outliers should be included in boxplot}

\item{dataMapping}{A \code{BoxWhiskerDataMapping} object mapping \code{x}, \code{y} and aesthetic groups to their variable names of \code{data}.}

\item{plotConfiguration}{An optional \code{BoxWhiskerConfiguration} object defining labels, grid, background and watermark.}

\item{plotObject}{An optional \code{ggplot} object on which to add the plot layer}
}
\value{
A \code{ggplot} object
}
\description{
Producing box-and-whisker plots
}
\examples{
# Produce box-and-whisker plots of log-normal distributed data
boxData <- data.frame(x = c(rep("A", 500), rep("B", 500)), y = rlnorm(1000))

plotBoxWhisker(data = boxData, dataMapping = BoxWhiskerDataMapping$new(x = "x", y = "y"))

# Remove outliers from boxplot
plotBoxWhisker(
  data = boxData,
  dataMapping = BoxWhiskerDataMapping$new(x = "x", y = "y"),
  outliers = FALSE
)

}
\references{
For examples, see:
\url{https://www.open-systems-pharmacology.org/TLF-Library/articles/box-whisker-vignette.html}
}
\seealso{
Other molecule plots: 
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
\code{\link{plotTimeProfile}()},
\code{\link{plotTornado}()}
}
\concept{molecule plots}
