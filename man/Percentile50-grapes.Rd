% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregation-input.R
\name{Percentile50\%}
\alias{Percentile50\%}
\title{Percentile50\%}
\usage{
`Percentile50\%`(x)
}
\arguments{
\item{x}{Numeric values}
}
\value{
Numeric value corresponding to \code{quantile(x, 50/100)}
}
\description{
Calculate \verb{Percentile50\%} i.e. \code{median} value
}
\examples{
# Calculate Percentile50\%
`Percentile50\%`(rnorm(1000))

}
\seealso{
Other stat functions: 
\code{\link{Percentile0\%}()},
\code{\link{Percentile100\%}()},
\code{\link{Percentile10\%}()},
\code{\link{Percentile15\%}()},
\code{\link{Percentile1\%}()},
\code{\link{Percentile2.5\%}()},
\code{\link{Percentile20\%}()},
\code{\link{Percentile25\%-1.5IQR}()},
\code{\link{Percentile25\%}()},
\code{\link{Percentile5\%}()},
\code{\link{Percentile75\%+1.5IQR}()},
\code{\link{Percentile75\%}()},
\code{\link{Percentile80\%}()},
\code{\link{Percentile85\%}()},
\code{\link{Percentile90\%}()},
\code{\link{Percentile95\%}()},
\code{\link{Percentile97.5\%}()},
\code{\link{Percentile99\%}()},
\code{\link{mean+1.96sd}()},
\code{\link{mean+sd}()},
\code{\link{mean-1.96sd}()},
\code{\link{mean-sd}()},
\code{\link{median+1.5IQR}()},
\code{\link{median+IQR}()},
\code{\link{median-1.5IQR}()},
\code{\link{median-IQR}()}
}
\concept{stat functions}
