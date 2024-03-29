% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-mapping.R
\name{getDefaultCaptions}
\alias{getDefaultCaptions}
\title{getDefaultCaptions}
\usage{
getDefaultCaptions(
  data,
  metaData = NULL,
  variableList = colnames(data),
  sep = "-"
)
}
\arguments{
\item{data}{data.frame used for legend caption}

\item{metaData}{list of lists containing metaData on \code{data}}

\item{variableList}{ordered vector of variables used for specifying the caption}

\item{sep}{characters separating variables in caption}
}
\value{
Factor levels corresponding to the legend captions
}
\description{
Creates default legend captions by concatenating the values of
the \code{data} and \code{metaData} of \code{variableList} variables from \code{data}.
}
\examples{

data <- data.frame(
  Population = c("Caucasian", "Asian", "Caucasian", "Asian"),
  Gender = c("Male", "Male", "Female", "Female"),
  Dose = c(50, 100, 100, 50),
  Compound = c("Midazolam", "Midazolam", "Midazolam", "Midazolam")
)

metaData <- list(Dose = list(unit = "mg"))

# Get captions using every variable of data
getDefaultCaptions(data, metaData)

# Get captions using specific variables of data
getDefaultCaptions(data, metaData, variableList = c("Gender", "Population"))

# Get captions separating variables witha space (character " ")
getDefaultCaptions(data, metaData, sep = " ")
}
