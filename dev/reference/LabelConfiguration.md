# LabelConfiguration

R6 class defining the configuration of the labels of a `ggplot` object

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

- `title`:

  `Label` object defining the title of the plot

- `subtitle`:

  `Label` object defining the subtitle of the plot

- `xlabel`:

  `Label` object defining the xlabel of the plot

- `ylabel`:

  `Label` object defining the ylabel of the plot

- `caption`:

  `Label` object defining the caption of the plot

- `y2label`:

  `Label` object defining the y2label of the plot

## Methods

### Public methods

- [`LabelConfiguration$new()`](#method-LabelConfiguration-new)

- [`LabelConfiguration$updatePlot()`](#method-LabelConfiguration-updatePlot)

- [`LabelConfiguration$clone()`](#method-LabelConfiguration-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `LabelConfiguration` object

#### Usage

    LabelConfiguration$new(
      title = NULL,
      subtitle = NULL,
      xlabel = NULL,
      ylabel = NULL,
      caption = NULL
    )

#### Arguments

- `title`:

  character or `Label` object defining title

- `subtitle`:

  character or `Label` object defining subtitle

- `xlabel`:

  character or `Label` object defining xlabel

- `ylabel`:

  character or `Label` object defining ylabel

- `caption`:

  character or `Label` object defining caption

#### Returns

A new `LabelConfiguration` object

------------------------------------------------------------------------

### Method `updatePlot()`

Update labels of a `ggplot` object and their properties

#### Usage

    LabelConfiguration$updatePlot(plotObject)

#### Arguments

- `plotObject`:

  a `ggplot` object

#### Returns

A `ggplot` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LabelConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
