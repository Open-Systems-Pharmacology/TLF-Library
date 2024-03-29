% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datamapping-groupmapping.R
\name{GroupMapping}
\alias{GroupMapping}
\title{GroupMapping}
\description{
R6 class for mapping \code{Grouping} variables to \code{data}
}
\seealso{
Other DataMapping classes: 
\code{\link{BoxWhiskerDataMapping}},
\code{\link{CumulativeTimeProfileDataMapping}},
\code{\link{DDIRatioDataMapping}},
\code{\link{Grouping}},
\code{\link{HistogramDataMapping}},
\code{\link{ObsVsPredDataMapping}},
\code{\link{ObservedDataMapping}},
\code{\link{PKRatioDataMapping}},
\code{\link{PieChartDataMapping}},
\code{\link{QQDataMapping}},
\code{\link{RangeDataMapping}},
\code{\link{ResVsPredDataMapping}},
\code{\link{ResVsTimeDataMapping}},
\code{\link{TimeProfileDataMapping}},
\code{\link{TornadoDataMapping}},
\code{\link{XYDataMapping}},
\code{\link{XYGDataMapping}}
}
\concept{DataMapping classes}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{color}}{R6 class \code{Grouping} object}

\item{\code{fill}}{R6 class \code{Grouping} object}

\item{\code{linetype}}{R6 class \code{Grouping} object}

\item{\code{shape}}{R6 class \code{Grouping} object}

\item{\code{size}}{R6 class \code{Grouping} object}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-GroupMapping-new}{\code{GroupMapping$new()}}
\item \href{#method-GroupMapping-clone}{\code{GroupMapping$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GroupMapping-new"></a>}}
\if{latex}{\out{\hypertarget{method-GroupMapping-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{GroupMapping} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GroupMapping$new(
  color = NULL,
  fill = NULL,
  linetype = NULL,
  shape = NULL,
  size = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{color}}{R6 class \code{Grouping} object or its input}

\item{\code{fill}}{R6 class \code{Grouping} object or its input}

\item{\code{linetype}}{R6 class \code{Grouping} object or its input}

\item{\code{shape}}{R6 class \code{Grouping} object or its input}

\item{\code{size}}{R6 class \code{Grouping} object or its input}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{GroupMapping} object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GroupMapping-clone"></a>}}
\if{latex}{\out{\hypertarget{method-GroupMapping-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GroupMapping$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
