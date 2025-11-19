# HistogramDataMapping

R6 class for mapping `x`, `bins`, `binwidth`,`stack` and `distribution`
to `data`

## See also

Other DataMapping classes:
[`BoxWhiskerDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/BoxWhiskerDataMapping.md),
[`CumulativeTimeProfileDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/CumulativeTimeProfileDataMapping.md),
[`DDIRatioDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/DDIRatioDataMapping.md),
[`GroupMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/GroupMapping.md),
[`Grouping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Grouping.md),
[`ObsVsPredDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ObsVsPredDataMapping.md),
[`ObservedDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ObservedDataMapping.md),
[`PKRatioDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/PKRatioDataMapping.md),
[`PieChartDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/PieChartDataMapping.md),
[`QQDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/QQDataMapping.md),
[`RangeDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/RangeDataMapping.md),
[`ResVsPredDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ResVsPredDataMapping.md),
[`ResVsTimeDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ResVsTimeDataMapping.md),
[`TimeProfileDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/TimeProfileDataMapping.md),
[`TornadoDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/TornadoDataMapping.md),
[`XYDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/XYDataMapping.md),
[`XYGDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/XYGDataMapping.md)

## Super classes

[`tlf::XYDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/XYDataMapping.md)
-\>
[`tlf::XYGDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/XYGDataMapping.md)
-\> `HistogramDataMapping`

## Public fields

- `frequency`:

  logical defining if histogram displays a frequency in y axis

- `stack`:

  logical defining if histogram bars should be stacked

- `bins`:

  number of bins or binning values/methods passed on
  [`ggplot2::geom_histogram`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)

- `binwidth`:

  width of bins passed on
  [`ggplot2::geom_histogram`](https://ggplot2.tidyverse.org/reference/geom_histogram.html).
  Overwrites `bins`

- `distribution`:

  Name of distribution to fit to the data. Only 2 distributions are
  currently available: `"normal"` and `"logNormal"`

## Methods

### Public methods

- [`HistogramDataMapping$new()`](#method-HistogramDataMapping-new)

- [`HistogramDataMapping$clone()`](#method-HistogramDataMapping-clone)

Inherited methods

- [`tlf::XYGDataMapping$checkMapData()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/XYGDataMapping.html#method-checkMapData)

------------------------------------------------------------------------

### Method `new()`

Create a new `HistogramDataMapping` object

#### Usage

    HistogramDataMapping$new(
      frequency = FALSE,
      stack = FALSE,
      bins = NULL,
      binwidth = NULL,
      distribution = NULL,
      ...
    )

#### Arguments

- `frequency`:

  logical defining if histogram displays a frequency in y axis

- `stack`:

  logical defining if histogram bars should be stacked

- `bins`:

  argument passed on
  [`ggplot2::geom_histogram`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)

- `binwidth`:

  width of bins passed on
  [`ggplot2::geom_histogram`](https://ggplot2.tidyverse.org/reference/geom_histogram.html).
  Overwrites `bins`

- `distribution`:

  Name of distribution to fit to the data. Only 2 distributions are
  currently available: `"normal"` and `"logNormal"`

- `...`:

  parameters inherited from `XYGDataMapping`

#### Returns

A new `HistogramDataMapping` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    HistogramDataMapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
