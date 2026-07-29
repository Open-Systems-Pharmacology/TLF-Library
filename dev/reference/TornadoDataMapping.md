# TornadoDataMapping

R6 class for mapping `values`, `labels` to `data`

## See also

Other DataMapping classes:
[`BoxWhiskerDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/BoxWhiskerDataMapping.md),
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
[`XYDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XYDataMapping.md),
[`XYGDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XYGDataMapping.md)

## Super classes

[`tlf::XYDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XYDataMapping.md)
-\>
[`tlf::XYGDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XYGDataMapping.md)
-\> `TornadoDataMapping`

## Public fields

- `lines`:

  numeric vector of limits to plot

- `sorted`:

  logical indicating if values should be sorted

## Methods

### Public methods

- [`TornadoDataMapping$new()`](#method-TornadoDataMapping-new)

- [`TornadoDataMapping$checkMapData()`](#method-TornadoDataMapping-checkMapData)

- [`TornadoDataMapping$clone()`](#method-TornadoDataMapping-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `TornadoDataMapping` object

#### Usage

    TornadoDataMapping$new(
      lines = DefaultDataMappingValues$tornado,
      sorted = NULL,
      x = NULL,
      y = NULL,
      ...
    )

#### Arguments

- `lines`:

  numeric vector of limits to plot

- `sorted`:

  logical indicating if values should be sorted

- `x`:

  Variable including the values of tornado plot

- `y`:

  Variable including the labels of tornado plot

- `...`:

  parameters inherited from `XYGDataMapping`

#### Returns

A new `TornadoDataMapping` object

------------------------------------------------------------------------

### Method `checkMapData()`

Check that `data` variables include map variables

#### Usage

    TornadoDataMapping$checkMapData(data, metaData = NULL)

#### Arguments

- `data`:

  data.frame to check

- `metaData`:

  list containing information on `data`

#### Returns

A data.frame with map and `defaultAes` variables. Dummy variable
`defaultAes` is necessary to allow further modification of plots.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TornadoDataMapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
