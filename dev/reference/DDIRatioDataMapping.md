# DDIRatioDataMapping

R6 class for mapping `x`, `y`, `GroupMapping` and DDI ratio `lines`
variables to `data`

## See also

Other DataMapping classes:
[`BoxWhiskerDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/BoxWhiskerDataMapping.md),
[`CumulativeTimeProfileDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/CumulativeTimeProfileDataMapping.md),
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
-\>
[`tlf::PKRatioDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/PKRatioDataMapping.md)
-\> `DDIRatioDataMapping`

## Active bindings

- `deltaGuest`:

  Value of `delta` in [Guest et
  al.](https://dmd.aspetjournals.org/content/39/2/170) equation

- `minRange`:

  Minimum range of x values for guest and ratio lines

- `residualsVsObserved`:

  Logical defining if calculated DDI data are as residuals vs observed
  or predicted vs observed

## Methods

### Public methods

- [`DDIRatioDataMapping$new()`](#method-DDIRatioDataMapping-new)

- [`DDIRatioDataMapping$clone()`](#method-DDIRatioDataMapping-clone)

Inherited methods

- [`tlf::PKRatioDataMapping$checkMapData()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/PKRatioDataMapping.html#method-checkMapData)

------------------------------------------------------------------------

### Method `new()`

Create a new `DDIRatioDataMapping` object

#### Usage

    DDIRatioDataMapping$new(
      deltaGuest = NULL,
      minRange = c(0.01, 100),
      lines = DefaultDataMappingValues$ddiRatio,
      residualsVsObserved = FALSE,
      ...
    )

#### Arguments

- `deltaGuest`:

  Value of `delta` in [Guest et
  al.](https://dmd.aspetjournals.org/content/39/2/170) equation. Default
  value is 1.

- `minRange`:

  Minimum range of x values for guest and ratio lines Default is
  \[0.01 - 100\]

- `lines`:

  List of ratio limits to display as diagonal/horizontal lines

- `residualsVsObserved`:

  Logical defining if calculated DDI data are as residuals vs observed
  or predicted vs observed

- `...`:

  parameters inherited from `PKRatioDataMapping`

#### Returns

A new `DDIRatioDataMapping` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DDIRatioDataMapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
