# PKRatioDataMapping

R6 class for mapping `x`, `y`, `GroupMapping` and pkRatio `lines`
variables to `data`

## See also

Other DataMapping classes:
[`BoxWhiskerDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/BoxWhiskerDataMapping.md),
[`CumulativeTimeProfileDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/CumulativeTimeProfileDataMapping.md),
[`DDIRatioDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/DDIRatioDataMapping.md),
[`GroupMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/GroupMapping.md),
[`Grouping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Grouping.md),
[`HistogramDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/HistogramDataMapping.md),
[`ObsVsPredDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ObsVsPredDataMapping.md),
[`ObservedDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ObservedDataMapping.md),
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
-\> `PKRatioDataMapping`

## Public fields

- `lines`:

  list of ratio limits to plot as horizontal lines

- `ymin`:

  mapping of upper value of error bars around scatter points

- `ymax`:

  mapping of lower value of error bars around scatter points

## Methods

### Public methods

- [`PKRatioDataMapping$new()`](#method-PKRatioDataMapping-new)

- [`PKRatioDataMapping$checkMapData()`](#method-PKRatioDataMapping-checkMapData)

- [`PKRatioDataMapping$clone()`](#method-PKRatioDataMapping-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `PKRatioDataMapping` object

#### Usage

    PKRatioDataMapping$new(
      x = NULL,
      y = NULL,
      ymin = NULL,
      ymax = NULL,
      lines = DefaultDataMappingValues$pkRatio,
      ...
    )

#### Arguments

- `x`:

  Name of x variable to map

- `y`:

  Name of y variable to map

- `ymin`:

  mapping of upper value of error bars around scatter points

- `ymax`:

  mapping of lower value of error bars around scatter points

- `lines`:

  List of ratio limits to display as horizontal lines

- `...`:

  parameters inherited from `XYGDataMapping`

#### Returns

A new `PKRatioDataMapping` object

------------------------------------------------------------------------

### Method `checkMapData()`

Check that `data` variables include map variables

#### Usage

    PKRatioDataMapping$checkMapData(data, metaData = NULL)

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

    PKRatioDataMapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
