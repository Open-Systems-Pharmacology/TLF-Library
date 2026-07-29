# XYGDataMapping

R6 class for mapping `x`, `y` and `GroupMapping` variables to `data`

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
[`TornadoDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/TornadoDataMapping.md),
[`XYDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XYDataMapping.md)

## Super class

[`tlf::XYDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XYDataMapping.md)
-\> `XYGDataMapping`

## Public fields

- `groupMapping`:

  R6 class `GroupMapping` object

## Methods

### Public methods

- [`XYGDataMapping$new()`](#method-XYGDataMapping-new)

- [`XYGDataMapping$checkMapData()`](#method-XYGDataMapping-checkMapData)

- [`XYGDataMapping$clone()`](#method-XYGDataMapping-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `XYGDataMapping` object

#### Usage

    XYGDataMapping$new(
      x = NULL,
      y = NULL,
      groupMapping = NULL,
      color = NULL,
      fill = NULL,
      linetype = NULL,
      shape = NULL,
      size = NULL,
      group = NULL,
      data = NULL
    )

#### Arguments

- `x`:

  Name of x variable to map

- `y`:

  Name of y variable to map

- `groupMapping`:

  R6 class `GroupMapping` object

- `color`:

  R6 class `Grouping` object or its input

- `fill`:

  R6 class `Grouping` object or its input

- `linetype`:

  R6 class `Grouping` object or its input

- `shape`:

  R6 class `Grouping` object or its input

- `size`:

  R6 class `Grouping` object or its input

- `group`:

  R6 class `Grouping` object or its input

- `data`:

  data.frame to map used by `.smartMapping`

#### Returns

A new `XYGDataMapping` object

------------------------------------------------------------------------

### Method `checkMapData()`

Check that `data` variables include map variables

#### Usage

    XYGDataMapping$checkMapData(data, metaData = NULL)

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

    XYGDataMapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
