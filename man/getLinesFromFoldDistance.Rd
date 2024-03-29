% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-mapping.R
\name{getLinesFromFoldDistance}
\alias{getLinesFromFoldDistance}
\title{getLinesFromFoldDistance}
\usage{
getLinesFromFoldDistance(foldDistance)
}
\arguments{
\item{foldDistance}{Numeric values
\strong{Caution}: this argument is meant for log scaled plots and since fold distance is a ratio it is expected positive.
In particular, line of identity corresponds to a \code{foldDistance} of \code{1}.}
}
\value{
A list of numeric values
}
\description{
Get list of values to provide to \code{lines} argument of \code{dataMapping} objects.
The \code{lines} are internally used as argument of \code{geom_hline} and \code{geom_abline} from \code{ggplot2}
}
\examples{

# Get lines for identity and 2-fold distance
getLinesFromFoldDistance(c(1, 2))

# Create dataMapping with lines identity and 2-fold distance
dataMapping <- ObsVsPredDataMapping$new(
  x = "predicted",
  y = "observed",
  lines = getLinesFromFoldDistance(c(1, 2))
)
}
