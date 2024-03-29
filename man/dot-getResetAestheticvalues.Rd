% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-aesthetics.R
\name{.getResetAestheticvalues}
\alias{.getResetAestheticvalues}
\title{.getResetAestheticvalues}
\usage{
.getResetAestheticvalues(n, map)
}
\arguments{
\item{n}{integer defining size of returned aesthetic vector}

\item{map}{Aesthetic (e.g. color, shape, linetype) map from \code{Theme} object.}
}
\value{
Vector of aesthetics
}
\description{
Get the aesthetic values (e.g color, shape, linetype) from an aesthetic map defined in \code{Theme}.
Reset the value every time it is used.
}
\keyword{internal}
