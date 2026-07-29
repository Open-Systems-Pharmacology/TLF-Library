# TimeProfilePlotConfiguration

R6 class defining the configuration of a `ggplot` object for time
profile plots

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
[`TornadoPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/TornadoPlotConfiguration.md),
[`XAxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XAxisConfiguration.md),
[`YAxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/YAxisConfiguration.md)

## Super class

[`tlf::PlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/PlotConfiguration.md)
-\> `TimeProfilePlotConfiguration`

## Public fields

- `lloqDirection`:

  Whether to draw LLOQ lines for x (vertical), y (horizontal) or x and y
  (both).

## Active bindings

- `y2Axis`:

  `YAxisConfiguration` object defining properties of y2-axis

## Methods

### Public methods

- [`TimeProfilePlotConfiguration$new()`](#method-TimeProfilePlotConfiguration-new)

- [`TimeProfilePlotConfiguration$clone()`](#method-TimeProfilePlotConfiguration-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `TimeProfilePlotConfiguration` object

#### Usage

    TimeProfilePlotConfiguration$new(
      ...,
      y2label = NULL,
      y2Axis = NULL,
      y2Scale = NULL,
      y2ValuesLimits = NULL,
      y2AxisLimits = NULL,
      y2Limits = lifecycle::deprecated(),
      lloqDirection = "horizontal",
      data = NULL,
      metaData = NULL,
      dataMapping = NULL
    )

#### Arguments

- `...`:

  parameters inherited from `PlotConfiguration`

- `y2label`:

  character or `Label` object defining plot y2label

- `y2Axis`:

  `YAxisConfiguration` object defining y-axis properties

- `y2Scale`:

  name of y2-axis scale. Use enum `Scaling` to access predefined scales.

- `y2ValuesLimits`:

  numeric vector of length 2 defining y values limits

- `y2AxisLimits`:

  numeric vector of length 2 defining y axis limits

- `y2Limits`:

  **\[deprecated\]**. Replaced by y2AxisLimits argument.

- `lloqDirection`:

  Whether to draw LLOQ lines for x (vertical), y (horizontal) or x and y
  (both).

- `data`:

  data.frame used by `.smartMapping`

- `metaData`:

  list of information on `data`

- `dataMapping`:

  R6 class or subclass `TimeProfileDataMapping`

#### Returns

A new `TimeProfilePlotConfiguration` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TimeProfilePlotConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
