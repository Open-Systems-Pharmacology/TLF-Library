% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotconfiguration-axis.R
\name{AxisConfiguration}
\alias{AxisConfiguration}
\title{AxisConfiguration}
\description{
R6 class defining the configuration of axis
}
\seealso{
Other PlotConfiguration classes: 
\code{\link{BackgroundConfiguration}},
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
\item{\code{valuesLimits}}{numeric vector of length 2 defining limits of axis.
A value of \code{NULL} is allowed and lead to default \code{ggplot2} behaviour}

\item{\code{axisLimits}}{numeric vector of length 2 defining limits of axis.
A value of \code{NULL} is allowed and lead to default \code{ggplot2} behaviour}

\item{\code{scale}}{name of axis scale from Enum \code{Scaling}
A value of \code{NULL} is allowed and will lead to a default linear scale}

\item{\code{ticks}}{function or values defining where axis ticks are placed}

\item{\code{minorTicks}}{function or values defining where axis minor ticks are placed}

\item{\code{ticklabels}}{function or values defining the axis tick labels}

\item{\code{font}}{\code{Font} object defining the font of the ticklabels}

\item{\code{expand}}{logical defining if data is expanded until axis.
If \code{TRUE}, data is expanded until axis
If \code{FALSE}, some space between data and axis is kept}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-AxisConfiguration-new}{\code{AxisConfiguration$new()}}
\item \href{#method-AxisConfiguration-ggplotScale}{\code{AxisConfiguration$ggplotScale()}}
\item \href{#method-AxisConfiguration-ggplotExpansion}{\code{AxisConfiguration$ggplotExpansion()}}
\item \href{#method-AxisConfiguration-prettyTicks}{\code{AxisConfiguration$prettyTicks()}}
\item \href{#method-AxisConfiguration-prettyMinorTicks}{\code{AxisConfiguration$prettyMinorTicks()}}
\item \href{#method-AxisConfiguration-prettyTickLabels}{\code{AxisConfiguration$prettyTickLabels()}}
\item \href{#method-AxisConfiguration-clone}{\code{AxisConfiguration$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AxisConfiguration-new"></a>}}
\if{latex}{\out{\hypertarget{method-AxisConfiguration-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{AxisConfiguration} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AxisConfiguration$new(
  valuesLimits = NULL,
  axisLimits = NULL,
  limits = lifecycle::deprecated(),
  scale = Scaling$lin,
  ticks = NULL,
  ticklabels = NULL,
  minorTicks = NULL,
  font = NULL,
  expand = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{valuesLimits}}{numeric vector of value limits (data outside these limits is removed)}

\item{\code{axisLimits}}{numeric vector of axis limits (data outside these limits is kept but not plotted)}

\item{\code{limits}}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}. Replaced by axisLimits argument.}

\item{\code{scale}}{character defining axis scale
Use enum \code{Scaling} to access predefined scales.}

\item{\code{ticks}}{numeric vector or function defining where to position axis ticks}

\item{\code{ticklabels}}{character vector or function defining what to print on axis ticks}

\item{\code{minorTicks}}{numeric vector or function defining where to position minor axis ticks}

\item{\code{font}}{\code{Font} object defining the font of ticklabels}

\item{\code{expand}}{logical defining if data is expanded until axis.
If \code{TRUE}, data is expanded until axis
If \code{FALSE}, some space between data and axis is kept}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{AxisConfiguration} object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AxisConfiguration-ggplotScale"></a>}}
\if{latex}{\out{\hypertarget{method-AxisConfiguration-ggplotScale}{}}}
\subsection{Method \code{ggplotScale()}}{
Get the \code{ggplot2} actual \code{trans} name of scale
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AxisConfiguration$ggplotScale()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A character included in \code{ggplot2} available \code{trans} names
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AxisConfiguration-ggplotExpansion"></a>}}
\if{latex}{\out{\hypertarget{method-AxisConfiguration-ggplotExpansion}{}}}
\subsection{Method \code{ggplotExpansion()}}{
Get the \code{ggplot2} actual function for expansion
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AxisConfiguration$ggplotExpansion()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A \code{ggplot2} function
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AxisConfiguration-prettyTicks"></a>}}
\if{latex}{\out{\hypertarget{method-AxisConfiguration-prettyTicks}{}}}
\subsection{Method \code{prettyTicks()}}{
Get tick values for pretty default log plots
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AxisConfiguration$prettyTicks()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
User defined tick values or tlf default ticks
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AxisConfiguration-prettyMinorTicks"></a>}}
\if{latex}{\out{\hypertarget{method-AxisConfiguration-prettyMinorTicks}{}}}
\subsection{Method \code{prettyMinorTicks()}}{
Get tick values for pretty default log plots
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AxisConfiguration$prettyMinorTicks()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
User defined tick values or tlf default ticks
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AxisConfiguration-prettyTickLabels"></a>}}
\if{latex}{\out{\hypertarget{method-AxisConfiguration-prettyTickLabels}{}}}
\subsection{Method \code{prettyTickLabels()}}{
Get tick labels for pretty default log plots
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AxisConfiguration$prettyTickLabels()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
User defined tick labels or tlf default ticklabels
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AxisConfiguration-clone"></a>}}
\if{latex}{\out{\hypertarget{method-AxisConfiguration-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AxisConfiguration$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
