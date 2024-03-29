% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregation-input.R
\name{median+IQR}
\alias{median+IQR}
\title{median+IQR}
\usage{
`median+IQR`(x)
}
\arguments{
\item{x}{Numeric values}
}
\value{
Numeric value corresponding to \code{median(x)+iqr(x)}
}
\description{
Calculate \code{median+IQR}
}
\examples{
# Calculate median+IQR
`median+IQR`(rnorm(1000))

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
\code{\link{Percentile50\%}()},
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
\code{\link{median-1.5IQR}()},
\code{\link{median-IQR}()}
}
\concept{stat functions}
