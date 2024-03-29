% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/atom-plots.R
\name{addRibbon}
\alias{addRibbon}
\title{addRibbon}
\usage{
addRibbon(
  data = NULL,
  metaData = NULL,
  x = NULL,
  ymin = NULL,
  ymax = NULL,
  caption = NULL,
  fill = NULL,
  color = NULL,
  size = NULL,
  linetype = NULL,
  alpha = NULL,
  dataMapping = NULL,
  plotConfiguration = NULL,
  plotObject = NULL
)
}
\arguments{
\item{data}{A data.frame to use for plot.}

\item{metaData}{A named list of information about \code{data} such as the \code{dimension} and \code{unit} of its variables.}

\item{x}{Numeric values to plot along the \code{x} axis. Only used instead of \code{data} if \code{data} is \code{NULL}.}

\item{ymin}{Numeric values to plot along the \code{y} axis. Only used instead of \code{data} if \code{data} is \code{NULL}.}

\item{ymax}{Numeric values to plot along the \code{y} axis. Only used instead of \code{data} if \code{data} is \code{NULL}.}

\item{caption}{Optional character values defining the legend captions of the plot.}

\item{fill}{Optional character values defining the colors of the plot layer.
See \code{grDevices::colors()} to get names of colors}

\item{color}{Optional character values defining the colors of the plot layer.
See \code{grDevices::colors()} to get names of colors}

\item{size}{Optional numeric values defining the size of the plot layer.}

\item{linetype}{Optional character values defining the linetype of the plot layer.
See enum \code{Linetypes} to get names of linetype.}

\item{alpha}{Numeric value between 0 and 1 corresponding to transparency of the ribbon.
The closer to 0, the more transparent the ribbon is.
The closer to 1, the more opaque the ribbon is.}

\item{dataMapping}{A \code{RangeDataMapping} object mapping \code{x}, \code{ymin}, \code{ymax} and aesthetic groups to their variable names of \code{data}.}

\item{plotConfiguration}{An optional \code{PlotConfiguration} object defining labels, grid, background and watermark.}

\item{plotObject}{An optional \code{ggplot} object on which to add the plot layer}
}
\value{
A \code{ggplot} object
}
\description{
Add a ribbon layer to a \code{ggplot} object.
}
\examples{
# Add ribbon using x, ymin and ymax
addRibbon(
  x = c(1, 2, 1, 2, 3),
  ymin = c(5, 0, 2, 3, 4),
  ymax = c(6, 2, 6, 2.5, 5)
)

# Add ribbon using a data.frame
time <- seq(0, 30, 0.1)
ribbonData <- data.frame(x = time, ymin = cos(time) - 1, ymax = cos(time) + 1)

addRibbon(
  data = ribbonData,
  dataMapping = RangeDataMapping$new(x = "x", ymin = "ymin", ymax = "ymax")
)

# Or for simple cases a smart mapping will get directly x, ymin and ymax from data
addRibbon(data = ribbonData)

# Add a ribbon with caption
addRibbon(data = ribbonData, caption = "My ribbon plot")

# Add a ribbon with specific properties
addRibbon(data = ribbonData, fill = "blue", alpha = 0.5, caption = "My data")

# Add a ribbon with specific properties
p <- addRibbon(data = ribbonData, fill = "blue", alpha = 0.5, caption = "My data")
addRibbon(
  x = c(0, 1), ymin = c(-0.5, -0.5), ymax = c(0.5, 0.5),
  fill = "red", alpha = 1,
  plotObject = p
)

}
\references{
For examples, see:
\url{https://www.open-systems-pharmacology.org/TLF-Library/articles/atom-plots.html}
}
\seealso{
Other atom plots: 
\code{\link{addErrorbar}()},
\code{\link{addLine}()},
\code{\link{addScatter}()},
\code{\link{initializePlot}()}
}
\concept{atom plots}
