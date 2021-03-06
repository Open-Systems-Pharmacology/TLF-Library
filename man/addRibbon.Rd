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
  alpha = 0.8,
  dataMapping = NULL,
  plotConfiguration = NULL,
  plotObject = NULL
)
}
\arguments{
\item{data}{data.frame containing the ribbon endpoints to be plotted.}

\item{metaData}{list of information on \code{data} such as \code{dimension} and \code{unit} of their variables}

\item{x}{Mapping for x values.
If \code{data} is NULL or not input, \code{x} numeric values will be used as is for the plot.}

\item{ymin}{Mapping for ymin values.
If \code{data} is NULL or not input, \code{ymin} numeric values will be used as is for the plot.
In case there is no \code{x} mapping, \code{ymin} are the y-values of horizontal lines.
In case there is no \code{ymax} mapping, a value of 0 is assumed for \code{ymax}.}

\item{ymax}{Mapping for ymax values.
If \code{data} is NULL or not input, \code{ymax} numeric values will be used as is for the plot.
In case there is no \code{x} mapping, \code{ymax} are the y-values of horizontal lines.
In case there is no \code{ymin} mapping, a value of 0 is assumed for \code{ymin}.}

\item{caption}{vector of character strings defining the legend captions.
This parameter is optional: default value is NULL.}

\item{fill}{vector of character strings defining the color of the scatter points.
This parameter is optional: default value \code{NULL} will choose colors according to the current theme.}

\item{color}{vector of character strings defining the color of the scatter points.
This parameter is optional: default value \code{NULL} will choose colors according to the current theme.}

\item{size}{vector of numerical defining the sizes of the scatter points.
This parameter is optional: default value \code{NULL} will choose sizes according to the current theme.}

\item{linetype}{vector of character strings defining the linetype linking the scatter points.
This parameter is optional: default value \code{NULL} won't provide lines.}

\item{alpha}{transparency of the ribbon.
Numeric value between 0 and 1. Value of 0, the plot is transparent. Value of 1, the plot is opaque.
Default value for \code{alpha} is 0.8.}

\item{dataMapping}{\code{XYGDataMapping} class or subclass
mapping x, y and aesthetic variables to the variable names of \code{data}.}

\item{plotConfiguration}{\code{PlotConfiguration} object defining the label and background properties of the plot.}

\item{plotObject}{\code{ggplot} graphical object to which the line layer is added
This parameter is optional: the \code{tlf} library will initialize an empty plot if the parameter is NULL or not provided}
}
\value{
A \code{ggplot} graphical object
}
\description{
Add a ribbon layer to a \code{ggplot} graphical object.
Use optional argument \code{caption} to set legend caption.
Since \code{ggplot} manage aesthetic properties across all layers,
aesthetic properties defined in \code{plotConfiguration} will apply across all layers.
}
\examples{
# Add a horizontal ribbon to a previous plot
p <- addRibbon(ymin = -5, ymax = 5)

# Add a horizontal ribbon to a previous plot
p <- addLine(x = c(1, 2), y = c(0, 0))
p <- addRibbon(ymin = -5, ymax = 5, plotObject = p)

# Add a custom ribbon
time <- seq(0, 30, 0.01)
customData <- data.frame(x = time, y = cos(time), ymin = cos(time) - 0.5, ymax = cos(time) + 0.5)

p <- addRibbon(
  data = customData,
  dataMapping = RangeDataMapping$new(x = "x", ymin = "ymin", ymax = "ymax")
)

# Or for simple cases a smart mapping will get directly x and y from data
p <- addRibbon(data = customData)

# Add a ribbon with caption
pr <- addRibbon(data = customData, caption = "My plot with ribbon")

# Add a ribbon with different transparency
p <- addRibbon(data = customData, alpha = 1)

# Add a scatter to the ribbon
pScatter <- addScatter(data = customData, caption = "My plot with ribbon", plotObject = pr)

# Add a scatter to the ribbon
pLine <- addLine(data = customData, caption = "My plot with ribbon", plotObject = pr)
}
