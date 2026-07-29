# ExportConfiguration

R6 class defining properties for saving a `ggplot` object

## See also

Other PlotConfiguration classes:
[`AxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/AxisConfiguration.md),
[`BackgroundConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/BackgroundConfiguration.md),
[`BackgroundElement`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/BackgroundElement.md),
[`BoxWhiskerPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/BoxWhiskerPlotConfiguration.md),
[`CumulativeTimeProfilePlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/CumulativeTimeProfilePlotConfiguration.md),
[`DDIRatioPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/DDIRatioPlotConfiguration.md),
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

- `name`:

  character defining the name of the file to be saved (without
  extension)

- `path`:

  Path of the directory to save plot to: path and filename are combined
  to create the fully qualified file name. Defaults to the working
  directory.

- `format`:

  character defining the format of the file to be saved

- `width`:

  numeric values defining the width in `units` of the plot dimensions
  after saving

- `height`:

  numeric values defining the height in `units` of the plot dimensions
  after saving

- `units`:

  character defining the unit of the saving dimension

- `dpi`:

  (dots per inch) numeric value defining plot resolution

## Methods

### Public methods

- [`ExportConfiguration$new()`](#method-ExportConfiguration-new)

- [`ExportConfiguration$print()`](#method-ExportConfiguration-print)

- [`ExportConfiguration$getFileName()`](#method-ExportConfiguration-getFileName)

- [`ExportConfiguration$savePlot()`](#method-ExportConfiguration-savePlot)

- [`ExportConfiguration$convertPixels()`](#method-ExportConfiguration-convertPixels)

- [`ExportConfiguration$clone()`](#method-ExportConfiguration-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `ExportConfiguration` object

#### Usage

    ExportConfiguration$new(
      path = NULL,
      name = NULL,
      format = NULL,
      width = NULL,
      height = NULL,
      units = NULL,
      dpi = NULL
    )

#### Arguments

- `path`:

  Path of the directory to save plot to: path and filename are combined
  to create the fully qualified file name. Defaults to the working
  directory.

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

#### Returns

A new `ExportConfiguration` object

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print properties of export configuration

#### Usage

    ExportConfiguration$print()

#### Returns

Export configuration properties

------------------------------------------------------------------------

### Method `getFileName()`

Print the default exported file name from the export configuration

#### Usage

    ExportConfiguration$getFileName()

#### Returns

Default file name

------------------------------------------------------------------------

### Method [`savePlot()`](https://rdrr.io/r/grDevices/savePlot.html)

Save/Export a plot

#### Usage

    ExportConfiguration$savePlot(plotObject, fileName = NULL)

#### Arguments

- `plotObject`:

  A `ggplot` object

- `fileName`:

  character file name of the exported plot

#### Returns

The file name of the exported plot

------------------------------------------------------------------------

### Method `convertPixels()`

If unit is in pixels, convert all export dimensions to inches to keep
compatibility with older versions of ggplot2

#### Usage

    ExportConfiguration$convertPixels()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ExportConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
