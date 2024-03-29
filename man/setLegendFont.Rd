% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-legend.R
\name{setLegendFont}
\alias{setLegendFont}
\title{setLegendFont}
\usage{
setLegendFont(
  plotObject,
  color = NULL,
  size = NULL,
  fontFamily = NULL,
  fontFace = NULL,
  angle = NULL,
  align = NULL
)
}
\arguments{
\item{plotObject}{ggplot object}

\item{color}{character defining the color of legend font}

\item{size}{numeric defining the size of legend font}

\item{fontFamily}{character defining the family of legend font}

\item{fontFace}{character defining the legend font face as defined in helper enum \code{FontFaces}.}

\item{angle}{numeric defining the angle of legend font}

\item{align}{character defining the alignment of legend font as defined in helper enum \code{Alignments}.}
}
\value{
A ggplot object
}
\description{
Set legend font properties
}
