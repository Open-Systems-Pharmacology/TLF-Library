# RangeDataMapping

R6 class for mapping `x`, `ymin` and `ymax` variable to `data`

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
[`PKRatioDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/PKRatioDataMapping.md),
[`PieChartDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/PieChartDataMapping.md),
[`QQDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/QQDataMapping.md),
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
-\> `RangeDataMapping`

## Public fields

- `ymin`:

  Name of ymin variable to map

- `ymax`:

  Name of ymax variable to map

## Methods

### Public methods

- [`RangeDataMapping$new()`](#method-RangeDataMapping-new)

- [`RangeDataMapping$checkMapData()`](#method-RangeDataMapping-checkMapData)

- [`RangeDataMapping$clone()`](#method-RangeDataMapping-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `RangeDataMapping` object

#### Usage

    RangeDataMapping$new(
      x = NULL,
      ymin = NULL,
      ymax = NULL,
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

- `ymin`:

  Name of ymin variable to map

- `ymax`:

  Name of ymax variable to map

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

A new `RangeDataMapping` object

------------------------------------------------------------------------

### Method `checkMapData()`

Check that `data` variables include map variables

#### Usage

    RangeDataMapping$checkMapData(data, metaData = NULL)

#### Arguments

- `data`:

  data.frame to check

- `metaData`:

  list containing information on `data`

#### Returns

A data.frame with map and `legendLabels` variables. Dummy variable
`legendLabels` is necessary to allow further modification of plots.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RangeDataMapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
