# GroupMapping

R6 class for mapping `Grouping` variables to `data`

## See also

Other DataMapping classes:
[`BoxWhiskerDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/BoxWhiskerDataMapping.md),
[`CumulativeTimeProfileDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/CumulativeTimeProfileDataMapping.md),
[`DDIRatioDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/DDIRatioDataMapping.md),
[`Grouping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Grouping.md),
[`HistogramDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/HistogramDataMapping.md),
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

## Public fields

- `color`:

  R6 class `Grouping` object

- `fill`:

  R6 class `Grouping` object

- `linetype`:

  R6 class `Grouping` object

- `shape`:

  R6 class `Grouping` object

- `size`:

  R6 class `Grouping` object

## Methods

### Public methods

- [`GroupMapping$new()`](#method-GroupMapping-new)

- [`GroupMapping$clone()`](#method-GroupMapping-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `GroupMapping` object

#### Usage

    GroupMapping$new(
      color = NULL,
      fill = NULL,
      linetype = NULL,
      shape = NULL,
      size = NULL
    )

#### Arguments

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

#### Returns

A new `GroupMapping` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GroupMapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
