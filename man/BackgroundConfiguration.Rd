% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotconfiguration-background.R
\name{BackgroundConfiguration}
\alias{BackgroundConfiguration}
\title{BackgroundConfiguration}
\description{
R6 class defining the configuration of background
}
\seealso{
Other PlotConfiguration classes: 
\code{\link{AxisConfiguration}},
\code{\link{BackgroundElement}},
\code{\link{BoxWhiskerPlotConfiguration}},
\code{\link{CumulativeTimeProfilePlotConfiguration}},
\code{\link{DDIRatioPlotConfiguration}},
\code{\link{ExportConfiguration}},
\code{\link{HistogramPlotConfiguration}},
\code{\link{LabelConfiguration}},
\code{\link{LegendConfiguration}},
\code{\link{LineElement}},
\code{\link{ObsVsPredPlotConfiguration}},
\code{\link{PKRatioPlotConfiguration}},
\code{\link{PieChartPlotConfiguration}},
\code{\link{PlotConfiguration}},
\code{\link{PlotGridConfiguration}},
\code{\link{QQPlotConfiguration}},
\code{\link{ResVsPredPlotConfiguration}},
\code{\link{ResVsTimePlotConfiguration}},
\code{\link{TimeProfilePlotConfiguration}},
\code{\link{TornadoPlotConfiguration}},
\code{\link{XAxisConfiguration}},
\code{\link{YAxisConfiguration}}
}
\concept{PlotConfiguration classes}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{watermark}}{\code{Label} object}

\item{\code{plot}}{\code{BackgroundElement} object}

\item{\code{panel}}{\code{BackgroundElement} object}

\item{\code{xAxis}}{\code{LineElement} object}

\item{\code{yAxis}}{\code{LineElement} object}

\item{\code{y2Axis}}{\code{LineElement} object}

\item{\code{xGrid}}{\code{LineElement} object}

\item{\code{yGrid}}{\code{LineElement} object}

\item{\code{y2Grid}}{\code{LineElement} object}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-BackgroundConfiguration-new}{\code{BackgroundConfiguration$new()}}
\item \href{#method-BackgroundConfiguration-updatePlot}{\code{BackgroundConfiguration$updatePlot()}}
\item \href{#method-BackgroundConfiguration-clone}{\code{BackgroundConfiguration$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BackgroundConfiguration-new"></a>}}
\if{latex}{\out{\hypertarget{method-BackgroundConfiguration-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{BackgroundConfiguration} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BackgroundConfiguration$new(
  watermark = NULL,
  plot = NULL,
  panel = NULL,
  xAxis = NULL,
  yAxis = NULL,
  y2Axis = NULL,
  xGrid = NULL,
  yGrid = NULL,
  y2Grid = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{watermark}}{\code{Label} object defining properties of watermark}

\item{\code{plot}}{\code{BackgroundElement} object defining outside plot background properties}

\item{\code{panel}}{\code{BackgroundElement} object defining panel (inside of plot) background properties}

\item{\code{xAxis}}{\code{LineElement} object defining properties of x-axis}

\item{\code{yAxis}}{\code{LineElement} object defining properties of y-axis}

\item{\code{y2Axis}}{\code{LineElement} object defining properties of right y-axis}

\item{\code{xGrid}}{\code{LineElement} object defining properties of x-grid}

\item{\code{yGrid}}{\code{LineElement} object defining properties of y-grid}

\item{\code{y2Grid}}{\code{LineElement} object defining properties of right y-grid}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{BackgroundConfiguration} object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BackgroundConfiguration-updatePlot"></a>}}
\if{latex}{\out{\hypertarget{method-BackgroundConfiguration-updatePlot}{}}}
\subsection{Method \code{updatePlot()}}{
Update background a \code{ggplot} object from \code{BackgroundConfiguration} properties
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BackgroundConfiguration$updatePlot(plotObject)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{plotObject}}{a \code{ggplot} object}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{ggplot} object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BackgroundConfiguration-clone"></a>}}
\if{latex}{\out{\hypertarget{method-BackgroundConfiguration-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BackgroundConfiguration$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
