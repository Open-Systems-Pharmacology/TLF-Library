# BackgroundElement

R6 class defining the properties of background elements

## See also

Other PlotConfiguration classes:
[`AxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/AxisConfiguration.md),
[`BackgroundConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/BackgroundConfiguration.md),
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

## Public fields

- `fill`:

  character defining the color filling of the background element

- `color`:

  character defining the color of the background element frame/line

- `size`:

  numeric defining the size of the background element frame/line

- `linetype`:

  character defining the size of the background element frame/line

## Methods

### Public methods

- [`BackgroundElement$new()`](#method-BackgroundElement-new)

- [`BackgroundElement$createPlotElement()`](#method-BackgroundElement-createPlotElement)

- [`BackgroundElement$clone()`](#method-BackgroundElement-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `BackgroundElement` object

#### Usage

    BackgroundElement$new(fill = NULL, color = NULL, size = NULL, linetype = NULL)

#### Arguments

- `fill`:

  character color filling of the background element

- `color`:

  character color of the frame of the background element

- `size`:

  character size of the frame of the background element

- `linetype`:

  character linetype of the frame of the background element

#### Returns

A new `BackgroundElement` object

------------------------------------------------------------------------

### Method `createPlotElement()`

Create a
[`ggplot2::element_rect`](https://ggplot2.tidyverse.org/reference/element.html)
directly usable by
[`ggplot2::theme`](https://ggplot2.tidyverse.org/reference/theme.html).

#### Usage

    BackgroundElement$createPlotElement(
      fill = NULL,
      color = NULL,
      size = NULL,
      linetype = NULL
    )

#### Arguments

- `fill`:

  character color filling of the background element

- `color`:

  character color of the frame of the background element

- `size`:

  character size of the frame of the background element

- `linetype`:

  character linetype of the frame of the background element

#### Returns

An `element_rect` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BackgroundElement$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
