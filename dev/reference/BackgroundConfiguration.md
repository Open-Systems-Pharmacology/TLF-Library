# BackgroundConfiguration

R6 class defining the configuration of background

## See also

Other PlotConfiguration classes:
[`AxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/AxisConfiguration.md),
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

- `watermark`:

  `Label` object

- `plot`:

  `BackgroundElement` object

- `panel`:

  `BackgroundElement` object

- `xAxis`:

  `LineElement` object

- `yAxis`:

  `LineElement` object

- `y2Axis`:

  `LineElement` object

- `xGrid`:

  `LineElement` object

- `yGrid`:

  `LineElement` object

- `y2Grid`:

  `LineElement` object

## Methods

### Public methods

- [`BackgroundConfiguration$new()`](#method-BackgroundConfiguration-new)

- [`BackgroundConfiguration$updatePlot()`](#method-BackgroundConfiguration-updatePlot)

- [`BackgroundConfiguration$clone()`](#method-BackgroundConfiguration-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `BackgroundConfiguration` object

#### Usage

    BackgroundConfiguration$new(
      watermark = NULL,
      plot = NULL,
      panel = NULL,
      xAxis = NULL,
      yAxis = NULL,
      y2Axis = NULL,
      xGrid = NULL,
      yGrid = NULL,
      y2Grid = NULL
    )

#### Arguments

- `watermark`:

  `Label` object defining properties of watermark

- `plot`:

  `BackgroundElement` object defining outside plot background properties

- `panel`:

  `BackgroundElement` object defining panel (inside of plot) background
  properties

- `xAxis`:

  `LineElement` object defining properties of x-axis

- `yAxis`:

  `LineElement` object defining properties of y-axis

- `y2Axis`:

  `LineElement` object defining properties of right y-axis

- `xGrid`:

  `LineElement` object defining properties of x-grid

- `yGrid`:

  `LineElement` object defining properties of y-grid

- `y2Grid`:

  `LineElement` object defining properties of right y-grid

#### Returns

A new `BackgroundConfiguration` object

------------------------------------------------------------------------

### Method `updatePlot()`

Update background a `ggplot` object from `BackgroundConfiguration`
properties

#### Usage

    BackgroundConfiguration$updatePlot(plotObject)

#### Arguments

- `plotObject`:

  a `ggplot` object

#### Returns

A `ggplot` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BackgroundConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
