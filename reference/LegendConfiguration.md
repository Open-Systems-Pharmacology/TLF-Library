# LegendConfiguration

R6 class defining the legend configuration of a `ggplot` object

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
[`XAxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/XAxisConfiguration.md),
[`YAxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/YAxisConfiguration.md)

## Active bindings

- `caption`:

  of legend defined as data.frame with caption properties

- `position`:

  of legend as defined in Enum `LegendPositions`

- `font`:

  `Font` object defining the font of the legend

- `background`:

  `Background` object defining the background of the legend

- `title`:

  character defining title of the legend

## Methods

### Public methods

- [`LegendConfiguration$new()`](#method-LegendConfiguration-new)

- [`LegendConfiguration$updatePlot()`](#method-LegendConfiguration-updatePlot)

- [`LegendConfiguration$clone()`](#method-LegendConfiguration-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `LegendConfiguration` object

#### Usage

    LegendConfiguration$new(
      position = NULL,
      caption = NULL,
      title = NULL,
      font = NULL,
      background = NULL
    )

#### Arguments

- `position`:

  position of the legend as defined by enum `LegendPositions`

- `caption`:

  data.frame containing the properties of the legend caption

- `title`:

  character or `Label` object defining the title of the legend. A value
  of `NULL` removes the title.

- `font`:

  `Font` object defining the font of the legend caption

- `background`:

  `BackgroundElement` object defining the background of the legend

#### Returns

A new `LegendConfiguration` object

------------------------------------------------------------------------

### Method `updatePlot()`

Update legend configuration on a `ggplot` object

#### Usage

    LegendConfiguration$updatePlot(plotObject)

#### Arguments

- `plotObject`:

  `ggplot` object

#### Returns

A `ggplot` object with updated axis properties

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LegendConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
