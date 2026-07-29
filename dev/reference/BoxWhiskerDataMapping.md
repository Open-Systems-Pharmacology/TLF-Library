# BoxWhiskerDataMapping

R6 class for mapping `y`, `GroupMapping`, `boxWhiskerLimits` and
`outlierLimits` to `data`

## See also

Other DataMapping classes:
[`CumulativeTimeProfileDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/CumulativeTimeProfileDataMapping.md),
[`DDIRatioDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/DDIRatioDataMapping.md),
[`GroupMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/GroupMapping.md),
[`Grouping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/Grouping.md),
[`HistogramDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/HistogramDataMapping.md),
[`ObsVsPredDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/ObsVsPredDataMapping.md),
[`ObservedDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/ObservedDataMapping.md),
[`PKRatioDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/PKRatioDataMapping.md),
[`PieChartDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/PieChartDataMapping.md),
[`QQDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/QQDataMapping.md),
[`RangeDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/RangeDataMapping.md),
[`ResVsPredDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/ResVsPredDataMapping.md),
[`ResVsTimeDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/ResVsTimeDataMapping.md),
[`TimeProfileDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/TimeProfileDataMapping.md),
[`TornadoDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/TornadoDataMapping.md),
[`XYDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XYDataMapping.md),
[`XYGDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XYGDataMapping.md)

## Super classes

[`tlf::XYDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XYDataMapping.md)
-\>
[`tlf::XYGDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XYGDataMapping.md)
-\> `BoxWhiskerDataMapping`

## Public fields

- `outlierLimits`:

  List of `minOutlierLimit` and `maxOutlierLimit` functions outside
  which `data` is flagged as outlier

- `boxWhiskerLimits`:

  List of `ymin`, `lower`, `middle`, `upper` and `ymax` functions
  calculated on `data` to obtain box whiskers

## Methods

### Public methods

- [`BoxWhiskerDataMapping$new()`](#method-BoxWhiskerDataMapping-new)

- [`BoxWhiskerDataMapping$getBoxWhiskerLimits()`](#method-BoxWhiskerDataMapping-getBoxWhiskerLimits)

- [`BoxWhiskerDataMapping$getOutliers()`](#method-BoxWhiskerDataMapping-getOutliers)

- [`BoxWhiskerDataMapping$clone()`](#method-BoxWhiskerDataMapping-clone)

Inherited methods

- [`tlf::XYGDataMapping$checkMapData()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XYGDataMapping.html#method-checkMapData)

------------------------------------------------------------------------

### Method `new()`

Create a new `BoxWhiskerDataMapping` object

#### Usage

    BoxWhiskerDataMapping$new(
      x = NULL,
      y,
      ymin = tlfStatFunctions$`Percentile5%`,
      lower = tlfStatFunctions$`Percentile25%`,
      middle = tlfStatFunctions$`Percentile50%`,
      upper = tlfStatFunctions$`Percentile75%`,
      ymax = tlfStatFunctions$`Percentile95%`,
      minOutlierLimit = tlfStatFunctions$`Percentile25%-1.5IQR`,
      maxOutlierLimit = tlfStatFunctions$`Percentile75%+1.5IQR`,
      ...
    )

#### Arguments

- `x`:

  Name of x variable to map Default value is NULL in case of a unique
  box in the boxplot.

- `y`:

  Name of y variable to map

- `ymin`:

  Name of function used for calculating lower whisker. Default value is
  `Percentile5%`.

- `lower`:

  Name of function used for calculating lower line of box Default value
  is `Percentile25%`.

- `middle`:

  Name of function used for calculating middle line Default value is
  `Percentile55%`.

- `upper`:

  Name of function used for calculating upper line of box Default value
  is `Percentile75%`.

- `ymax`:

  Name of function used for calculating upper whisker Default value is
  `Percentile95%`.

- `minOutlierLimit`:

  Name of function used for calculating lower outlier limit Default
  value is `Percentile25-1.5IQR%`.

- `maxOutlierLimit`:

  Name of function used for calculating upper outlier limit Default
  value is `Percentile75+1.5IQR%`.

- `...`:

  parameters inherited from `XYGDataMapping`

#### Returns

A new `BoxWhiskerDataMapping` object

------------------------------------------------------------------------

### Method `getBoxWhiskerLimits()`

Get a data.frame with box-whisker limit by group

#### Usage

    BoxWhiskerDataMapping$getBoxWhiskerLimits(data)

#### Arguments

- `data`:

  data.frame to check

#### Returns

A data.frame with `ymin`, `lower`, `middle`, `upper`, `ymax` variables.

------------------------------------------------------------------------

### Method `getOutliers()`

Get a data.frame flagging outliers

#### Usage

    BoxWhiskerDataMapping$getOutliers(data)

#### Arguments

- `data`:

  data.frame to check

#### Returns

A data.frame with `minOutliers` and `maxOutliers` variables. Values not
flagged are `NA` in the outliers variables

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BoxWhiskerDataMapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
