# LineElement

R6 class defining the properties of background line elements

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

## Super class

[`tlf::BackgroundElement`](https://www.open-systems-pharmacology.org/TLF-Library/reference/BackgroundElement.md)
-\> `LineElement`

## Methods

### Public methods

- [`LineElement$createPlotElement()`](#method-LineElement-createPlotElement)

- [`LineElement$clone()`](#method-LineElement-clone)

Inherited methods

- [`tlf::BackgroundElement$initialize()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/BackgroundElement.html#method-initialize)

------------------------------------------------------------------------

### Method `createPlotElement()`

Create a
[`ggplot2::element_line`](https://ggplot2.tidyverse.org/reference/element.html)
directly usable by
[`ggplot2::theme`](https://ggplot2.tidyverse.org/reference/theme.html).

#### Usage

    LineElement$createPlotElement(color = NULL, size = NULL, linetype = NULL)

#### Arguments

- `color`:

  character color of the frame of the background element

- `size`:

  character size of the frame of the background element

- `linetype`:

  character linetype of the frame of the background element

#### Returns

An `element_line` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LineElement$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
