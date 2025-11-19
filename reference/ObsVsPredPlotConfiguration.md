# ObsVsPredPlotConfiguration

R6 class defining the configuration of a `ggplot` object for Obs vs Pred
plots

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

[`tlf::PlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/PlotConfiguration.md)
-\> `ObsVsPredPlotConfiguration`

## Public fields

- `defaultSymmetricAxes`:

  Default option setting symmetric xAxis and/or yAxis limits when
  creating a `ObsVsPredPlotConfiguration` object

- `lloqDirection`:

  Whether to draw LLOQ lines for x (vertical), y (horizontal) or x and y
  (both).

- `foldLinesLegend`:

  Whether to draw fold lines in legend. default to FALSE.

- `foldLinesLegendDiagonal`:

  Whether to draw diagonal lines in legend for fold lines. default to
  FALSE.

## Active bindings

- `foldLineslegendType`:

  translation of `foldLinesLegendDiagonal` in geom type.

## Methods

### Public methods

- [`ObsVsPredPlotConfiguration$new()`](#method-ObsVsPredPlotConfiguration-new)

- [`ObsVsPredPlotConfiguration$clone()`](#method-ObsVsPredPlotConfiguration-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `ObsVsPredPlotConfiguration` object

#### Usage

    ObsVsPredPlotConfiguration$new(
      lloqDirection = "vertical",
      foldLinesLegend = FALSE,
      foldLinesLegendDiagonal = FALSE,
      ...
    )

#### Arguments

- `lloqDirection`:

  Whether to draw LLOQ lines for x (vertical), y (horizontal) or x and y
  (both).

- `foldLinesLegend`:

  Whether to draw fold lines in legend. default to FALSE.

- `foldLinesLegendDiagonal`:

  Whether to draw diagonal lines in legend for fold lines. default to
  FALSE.

- `...`:

  parameters inherited from `PlotConfiguration`

#### Returns

A new `CumulativeTimeProfilePlotConfiguration` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ObsVsPredPlotConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
