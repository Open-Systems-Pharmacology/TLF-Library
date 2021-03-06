% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-background.R
\name{createWatermarkGrob}
\alias{createWatermarkGrob}
\title{createWatermarkGrob}
\usage{
createWatermarkGrob(label, alpha = NULL)
}
\arguments{
\item{label}{Character or Label object corresponding to the watermark text
(and its font properties if Label)}

\item{alpha}{Transparency of the watermark label.
Alpha is a numeric between 0 and 1: 0 label is totally transparent, 1 label is totally opaque.
Default alpha is defined from theme aesthetic alpha map}
}
\value{
Watermark background as a ggplot grob object
}
\description{
createWatermarkGrob creates a ggplot grob based on the label text and its font properties.
}
