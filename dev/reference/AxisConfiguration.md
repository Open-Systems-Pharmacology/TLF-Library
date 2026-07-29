# AxisConfiguration

R6 class defining the configuration of axis

## See also

Other PlotConfiguration classes:
[`BackgroundConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/BackgroundConfiguration.md),
[`BackgroundElement`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/BackgroundElement.md),
[`BoxWhiskerPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/BoxWhiskerPlotConfiguration.md),
[`CumulativeTimeProfilePlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/CumulativeTimeProfilePlotConfiguration.md),
[`DDIRatioPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/DDIRatioPlotConfiguration.md),
[`ExportConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/ExportConfiguration.md),
[`HistogramPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/HistogramPlotConfiguration.md),
[`LabelConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/LabelConfiguration.md),
[`LegendConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/LegendConfiguration.md),
[`LineElement`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/LineElement.md),
[`ObsVsPredPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/ObsVsPredPlotConfiguration.md),
[`PKRatioPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/PKRatioPlotConfiguration.md),
[`PieChartPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/PieChartPlotConfiguration.md),
[`PlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/PlotConfiguration.md),
[`PlotGridConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/PlotGridConfiguration.md),
[`QQPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/QQPlotConfiguration.md),
[`ResVsPredPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/ResVsPredPlotConfiguration.md),
[`ResVsTimePlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/ResVsTimePlotConfiguration.md),
[`TimeProfilePlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/TimeProfilePlotConfiguration.md),
[`TornadoPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/TornadoPlotConfiguration.md),
[`XAxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XAxisConfiguration.md),
[`YAxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/YAxisConfiguration.md)

## Active bindings

- `valuesLimits`:

  numeric vector of length 2 defining limits of axis. A value of `NULL`
  is allowed and lead to default `ggplot2` behaviour

- `axisLimits`:

  numeric vector of length 2 defining limits of axis. A value of `NULL`
  is allowed and lead to default `ggplot2` behaviour

- `scale`:

  name of axis scale from Enum `Scaling` A value of `NULL` is allowed
  and will lead to a default linear scale

- `ticks`:

  function or values defining where axis ticks are placed

- `minorTicks`:

  function or values defining where axis minor ticks are placed

- `ticklabels`:

  function or values defining the axis tick labels

- `font`:

  `Font` object defining the font of the ticklabels

- `expand`:

  logical defining if data is expanded until axis. If `TRUE`, data is
  expanded until axis If `FALSE`, some space between data and axis is
  kept

## Methods

### Public methods

- [`AxisConfiguration$new()`](#method-AxisConfiguration-new)

- [`AxisConfiguration$ggplotScale()`](#method-AxisConfiguration-ggplotScale)

- [`AxisConfiguration$ggplotExpansion()`](#method-AxisConfiguration-ggplotExpansion)

- [`AxisConfiguration$prettyTicks()`](#method-AxisConfiguration-prettyTicks)

- [`AxisConfiguration$prettyMinorTicks()`](#method-AxisConfiguration-prettyMinorTicks)

- [`AxisConfiguration$prettyTickLabels()`](#method-AxisConfiguration-prettyTickLabels)

- [`AxisConfiguration$clone()`](#method-AxisConfiguration-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `AxisConfiguration` object

#### Usage

    AxisConfiguration$new(
      valuesLimits = NULL,
      axisLimits = NULL,
      limits = lifecycle::deprecated(),
      scale = Scaling$lin,
      ticks = NULL,
      ticklabels = NULL,
      minorTicks = NULL,
      font = NULL,
      expand = FALSE
    )

#### Arguments

- `valuesLimits`:

  numeric vector of value limits (data outside these limits is removed)

- `axisLimits`:

  numeric vector of axis limits (data outside these limits is kept but
  not plotted)

- `limits`:

  **\[deprecated\]**. Replaced by axisLimits argument.

- `scale`:

  character defining axis scale Use enum `Scaling` to access predefined
  scales.

- `ticks`:

  numeric vector or function defining where to position axis ticks

- `ticklabels`:

  character vector or function defining what to print on axis ticks

- `minorTicks`:

  numeric vector or function defining where to position minor axis ticks

- `font`:

  `Font` object defining the font of ticklabels

- `expand`:

  logical defining if data is expanded until axis. If `TRUE`, data is
  expanded until axis If `FALSE`, some space between data and axis is
  kept

#### Returns

A new `AxisConfiguration` object

------------------------------------------------------------------------

### Method `ggplotScale()`

Get the `ggplot2` actual `trans` name of scale

#### Usage

    AxisConfiguration$ggplotScale()

#### Returns

A character included in `ggplot2` available `trans` names

------------------------------------------------------------------------

### Method `ggplotExpansion()`

Get the `ggplot2` actual function for expansion

#### Usage

    AxisConfiguration$ggplotExpansion()

#### Returns

A `ggplot2` function

------------------------------------------------------------------------

### Method `prettyTicks()`

Get tick values for pretty default log plots

#### Usage

    AxisConfiguration$prettyTicks()

#### Returns

User defined tick values or tlf default ticks

------------------------------------------------------------------------

### Method `prettyMinorTicks()`

Get tick values for pretty default log plots

#### Usage

    AxisConfiguration$prettyMinorTicks()

#### Returns

User defined tick values or tlf default ticks

------------------------------------------------------------------------

### Method `prettyTickLabels()`

Get tick labels for pretty default log plots

#### Usage

    AxisConfiguration$prettyTickLabels()

#### Returns

User defined tick labels or tlf default ticklabels

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AxisConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
