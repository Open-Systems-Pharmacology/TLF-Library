% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa-utilities.R
\name{.parseValueToObject}
\alias{.parseValueToObject}
\title{.parseValueToObject}
\usage{
.parseValueToObject(objectName, value)
}
\arguments{
\item{objectName}{Name of the object to update}

\item{value}{Value of the variable \code{objectName}}
}
\value{
An expression to \code{eval()}
}
\description{
Create an expression of type \code{objectName <- value}
}
\keyword{internal}
