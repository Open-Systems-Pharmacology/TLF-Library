# Grouping

R6 class for mapping a `group` of variable(s) and their `label` to
`data`

## See also

Other DataMapping classes:
[`BoxWhiskerDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/BoxWhiskerDataMapping.md),
[`CumulativeTimeProfileDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/CumulativeTimeProfileDataMapping.md),
[`DDIRatioDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/DDIRatioDataMapping.md),
[`GroupMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/GroupMapping.md),
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

- `group`:

  data.frame or character defining the groups or group variables to
  group by

- `label`:

  character printed name of the `grouping`

## Methods

### Public methods

- [`Grouping$new()`](#method-Grouping-new)

- [`Grouping$getCaptions()`](#method-Grouping-getCaptions)

- [`Grouping$clone()`](#method-Grouping-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `Grouping` object

#### Usage

    Grouping$new(group, label = NULL)

#### Arguments

- `group`:

  data.frame or character vector of groups

- `label`:

  character name of the `group`

#### Returns

A new `Grouping` object

------------------------------------------------------------------------

### Method `getCaptions()`

Get the caption associated to each `group`

#### Usage

    Grouping$getCaptions(data, metaData = NULL)

#### Arguments

- `data`:

  data.frame to map

- `metaData`:

  list of information on the `data`

#### Returns

A vector of characters containing the captions associated to each
`group` of `data`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Grouping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
