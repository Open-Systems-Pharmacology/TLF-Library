# YAxisConfiguration

R6 class defining the configuration of Y-axis

## See also

Other PlotConfiguration classes:
[`AxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/AxisConfiguration.md),
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
[`XAxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XAxisConfiguration.md)

## Super class

[`tlf::AxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/AxisConfiguration.md)
-\> `YAxisConfiguration`

## Public fields

- `position`:

  character position of the Y-axis

## Methods

### Public methods

- [`YAxisConfiguration$updatePlot()`](#method-YAxisConfiguration-updatePlot)

- [`YAxisConfiguration$clone()`](#method-YAxisConfiguration-clone)

Inherited methods

- [`tlf::AxisConfiguration$ggplotExpansion()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/AxisConfiguration.html#method-ggplotExpansion)
- [`tlf::AxisConfiguration$ggplotScale()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/AxisConfiguration.html#method-ggplotScale)
- [`tlf::AxisConfiguration$initialize()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/AxisConfiguration.html#method-initialize)
- [`tlf::AxisConfiguration$prettyMinorTicks()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/AxisConfiguration.html#method-prettyMinorTicks)
- [`tlf::AxisConfiguration$prettyTickLabels()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/AxisConfiguration.html#method-prettyTickLabels)
- [`tlf::AxisConfiguration$prettyTicks()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/AxisConfiguration.html#method-prettyTicks)

------------------------------------------------------------------------

### Method `updatePlot()`

Update axis configuration on a `ggplot` object

#### Usage

    YAxisConfiguration$updatePlot(
      plotObject,
      xAxisLimits = NULL,
      xlim = lifecycle::deprecated()
    )

#### Arguments

- `plotObject`:

  `ggplot` object

- `xAxisLimits`:

  limits of `x` axis to prevent `coord_cartesian` to overwrite its
  properties

- `xlim`:

  **\[deprecated\]**. Replaced by xAxisLimits argument.

#### Returns

A `ggplot` object with updated axis properties

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    YAxisConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
