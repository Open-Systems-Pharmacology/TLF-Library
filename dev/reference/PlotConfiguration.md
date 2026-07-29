# PlotConfiguration

R6 class defining the configuration of a `ggplot` object

## References

For examples, see:
<https://www.open-systems-pharmacology.org/TLF-Library/articles/plot-configuration.html>

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
[`PlotGridConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/PlotGridConfiguration.md),
[`QQPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/QQPlotConfiguration.md),
[`ResVsPredPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/ResVsPredPlotConfiguration.md),
[`ResVsTimePlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/ResVsTimePlotConfiguration.md),
[`TimeProfilePlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/TimeProfilePlotConfiguration.md),
[`TornadoPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/TornadoPlotConfiguration.md),
[`XAxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XAxisConfiguration.md),
[`YAxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/YAxisConfiguration.md)

## Public fields

- `export`:

  R6 class `ExportConfiguration` defining properties for
  saving/exporting plot

- `defaultXScale`:

  Default xAxis scale value when creating a `PlotConfiguration` object

- `defaultYScale`:

  Default yAxis scale value when creating a `PlotConfiguration` object

- `defaultExpand`:

  Default expand value when creating a `PlotConfiguration` object

- `defaultSymmetricAxes`:

  Default option setting symmetric xAxis and/or yAxis limits when
  creating a `PlotConfiguration` object

## Active bindings

- `labels`:

  `LabelConfiguration` object defining properties of labels

- `legend`:

  `LegendConfiguration` object defining properties of legend

- `xAxis`:

  `XAxisConfiguration` object defining properties of x-axis

- `yAxis`:

  `YAxisConfiguration` object defining properties of x-axis

- `background`:

  `BackgroundConfiguration` object defining properties of x-axis

- `lines`:

  `ThemeAestheticSelections` defining properties of lines

- `ribbons`:

  `ThemeAestheticSelections` defining properties of ribbons

- `points`:

  `ThemeAestheticSelections` defining properties of points

- `errorbars`:

  `ThemeAestheticSelections` defining properties of error bars

## Methods

### Public methods

- [`PlotConfiguration$new()`](#method-PlotConfiguration-new)

- [`PlotConfiguration$clone()`](#method-PlotConfiguration-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `PlotConfiguration` object

#### Usage

    PlotConfiguration$new(
      title = NULL,
      subtitle = NULL,
      xlabel = NULL,
      ylabel = NULL,
      caption = NULL,
      legend = NULL,
      legendTitle = NULL,
      legendPosition = NULL,
      xAxis = NULL,
      xScale = NULL,
      xValuesLimits = NULL,
      xAxisLimits = NULL,
      xLimits = lifecycle::deprecated(),
      yAxis = NULL,
      yScale = NULL,
      yValuesLimits = NULL,
      yAxisLimits = NULL,
      yLimits = lifecycle::deprecated(),
      background = NULL,
      plotArea = NULL,
      panelArea = NULL,
      xGrid = NULL,
      yGrid = NULL,
      watermark = NULL,
      lines = NULL,
      points = NULL,
      ribbons = NULL,
      errorbars = NULL,
      export = NULL,
      name = NULL,
      format = NULL,
      width = NULL,
      height = NULL,
      units = NULL,
      dpi = NULL,
      data = NULL,
      metaData = NULL,
      dataMapping = NULL
    )

#### Arguments

- `title`:

  character or `Label` object defining plot title

- `subtitle`:

  character or `Label` object defining plot subtitle

- `xlabel`:

  character or `Label` object defining plot xlabel

- `ylabel`:

  character or `Label` object defining plot ylabel

- `caption`:

  character or `Label` object defining plot caption

- `legend`:

  `LegendConfiguration` object defining legend properties

- `legendTitle`:

  character or `Label` object defining legend title

- `legendPosition`:

  character defining legend position. Use Enum `LegendPositions` to get
  a list of available to legend positions.

- `xAxis`:

  `XAxisConfiguration` object defining x-axis properties

- `xScale`:

  name of X-axis scale. Use enum `Scaling` to access predefined scales.

- `xValuesLimits`:

  numeric vector of length 2 defining x values limits

- `xAxisLimits`:

  numeric vector of length 2 defining x-axis limits

- `xLimits`:

  **\[deprecated\]**. Replaced by xAxisLimits argument.

- `yAxis`:

  `YAxisConfiguration` object defining y-axis properties

- `yScale`:

  name of y-axis scale. Use enum `Scaling` to access predefined scales.

- `yValuesLimits`:

  numeric vector of length 2 defining x values limits

- `yAxisLimits`:

  numeric vector of length 2 defining x-axis limits

- `yLimits`:

  **\[deprecated\]**. Replaced by yAxisLimits argument.

- `background`:

  `BackgroundConfiguration` object defining background properties

- `plotArea`:

  `BackgroundElement` object defining properties of plot area

- `panelArea`:

  `BackgroundElement` object defining properties of panel area

- `xGrid`:

  `LineElement` object defining properties of x-grid background

- `yGrid`:

  `LineElement` object defining properties of y-grid background

- `watermark`:

  character or `Label` object defining watermark

- `lines`:

  `ThemeAestheticSelections` object or list defining how lines are
  plotted

- `points`:

  `ThemeAestheticSelections` object or list defining how points are
  plotted

- `ribbons`:

  `ThemeAestheticSelections` object or list defining how ribbons are
  plotted

- `errorbars`:

  `ThemeAestheticSelections` object or list defining how errorbars are
  plotted

- `export`:

  R6 class `ExportConfiguration` defining properties for
  saving/exporting plot

- `name`:

  character defining the name of the file to be saved (without
  extension)

- `format`:

  character defining the format of the file to be saved.

- `width`:

  numeric values defining the width in `units` of the plot dimensions
  after saving

- `height`:

  numeric values defining the height in `units` of the plot dimensions
  after saving

- `units`:

  character defining the unit of the saving dimension

- `dpi`:

  numeric value defining plot resolution (dots per inch)

- `data`:

  data.frame used by `.smartMapping`

- `metaData`:

  list of information on `data`

- `dataMapping`:

  R6 class or subclass `XYDataMapping`

#### Returns

A new `PlotConfiguration` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PlotConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
