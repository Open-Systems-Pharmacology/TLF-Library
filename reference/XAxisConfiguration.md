# XAxisConfiguration

R6 class defining the configuration of X-axis

## See also

Other PlotConfiguration classes:
[`AxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/AxisConfiguration.md),
[`BackgroundConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/BackgroundConfiguration.md),
[`BackgroundElement`](https://www.open-systems-pharmacology.org/TLF-Library/reference/BackgroundElement.md),
[`BoxWhiskerPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/BoxWhiskerPlotConfiguration.md),
[`CumulativeTimeProfilePlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/CumulativeTimeProfilePlotConfiguration.md),
[`DDIRatioPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/DDIRatioPlotConfiguration.md),
[`ExportConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ExportConfiguration.md),
[`HistogramPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/HistogramPlotConfiguration.md),
[`LabelConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/LabelConfiguration.md),
[`LegendConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/LegendConfiguration.md),
[`LineElement`](https://www.open-systems-pharmacology.org/TLF-Library/reference/LineElement.md),
[`ObsVsPredPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ObsVsPredPlotConfiguration.md),
[`PKRatioPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/PKRatioPlotConfiguration.md),
[`PieChartPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/PieChartPlotConfiguration.md),
[`PlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/PlotConfiguration.md),
[`PlotGridConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/PlotGridConfiguration.md),
[`QQPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/QQPlotConfiguration.md),
[`ResVsPredPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ResVsPredPlotConfiguration.md),
[`ResVsTimePlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ResVsTimePlotConfiguration.md),
[`TimeProfilePlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/TimeProfilePlotConfiguration.md),
[`TornadoPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/TornadoPlotConfiguration.md),
[`YAxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/YAxisConfiguration.md)

## Super class

[`tlf::AxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/AxisConfiguration.md)
-\> `XAxisConfiguration`

## Methods

### Public methods

- [`XAxisConfiguration$updatePlot()`](#method-XAxisConfiguration-updatePlot)

- [`XAxisConfiguration$clone()`](#method-XAxisConfiguration-clone)

Inherited methods

- [`tlf::AxisConfiguration$ggplotExpansion()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/AxisConfiguration.html#method-ggplotExpansion)
- [`tlf::AxisConfiguration$ggplotScale()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/AxisConfiguration.html#method-ggplotScale)
- [`tlf::AxisConfiguration$initialize()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/AxisConfiguration.html#method-initialize)
- [`tlf::AxisConfiguration$prettyMinorTicks()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/AxisConfiguration.html#method-prettyMinorTicks)
- [`tlf::AxisConfiguration$prettyTickLabels()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/AxisConfiguration.html#method-prettyTickLabels)
- [`tlf::AxisConfiguration$prettyTicks()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/AxisConfiguration.html#method-prettyTicks)

------------------------------------------------------------------------

### Method `updatePlot()`

Update axis configuration on a `ggplot` object

#### Usage

    XAxisConfiguration$updatePlot(
      plotObject,
      yAxisLimits = NULL,
      ylim = lifecycle::deprecated()
    )

#### Arguments

- `plotObject`:

  `ggplot` object

- `yAxisLimits`:

  values of axisLimits for `y` axis to prevent `coord_cartesian` to
  overwrite its properties

- `ylim`:

  **\[deprecated\]**. Replaced by yAxisLimits argument.

#### Returns

A `ggplot` object with updated axis properties

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    XAxisConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
