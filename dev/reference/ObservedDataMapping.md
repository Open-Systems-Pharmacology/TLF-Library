# ObservedDataMapping

R6 class for mapping `x`, `y`, of observed data for a time profile plot

## See also

Other DataMapping classes:
[`BoxWhiskerDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/BoxWhiskerDataMapping.md),
[`CumulativeTimeProfileDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/CumulativeTimeProfileDataMapping.md),
[`DDIRatioDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/DDIRatioDataMapping.md),
[`GroupMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/GroupMapping.md),
[`Grouping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/Grouping.md),
[`HistogramDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/HistogramDataMapping.md),
[`ObsVsPredDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/ObsVsPredDataMapping.md),
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
-\> `ObservedDataMapping`

## Public fields

- `error`:

  mapping error bars around scatter points

- `mdv`:

  mapping missing dependent variable

- `ymin`:

  mapping error bars around scatter points

- `ymax`:

  mapping error bars around scatter points

- `y2Axis`:

  Name of y2Axis variable to map

- `lloq`:

  mapping lloq lines

## Methods

### Public methods

- [`ObservedDataMapping$new()`](#method-ObservedDataMapping-new)

- [`ObservedDataMapping$checkMapData()`](#method-ObservedDataMapping-checkMapData)

- [`ObservedDataMapping$requireDualAxis()`](#method-ObservedDataMapping-requireDualAxis)

- [`ObservedDataMapping$getLeftAxis()`](#method-ObservedDataMapping-getLeftAxis)

- [`ObservedDataMapping$getRightAxis()`](#method-ObservedDataMapping-getRightAxis)

- [`ObservedDataMapping$clone()`](#method-ObservedDataMapping-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `ObservedDataMapping` object

#### Usage

    ObservedDataMapping$new(
      x,
      y,
      ymin = NULL,
      ymax = NULL,
      y2Axis = NULL,
      group = NULL,
      color = NULL,
      shape = NULL,
      error = NULL,
      uncertainty = lifecycle::deprecated(),
      mdv = NULL,
      data = NULL,
      lloq = NULL
    )

#### Arguments

- `x`:

  Name of x variable to map

- `y`:

  Name of y variable to map

- `ymin`:

  mapping lower end of error bars around scatter points

- `ymax`:

  mapping upper end of error bars around scatter points

- `y2Axis`:

  Name of y2Axis variable to map

- `group`:

  R6 class `Grouping` object or its input

- `color`:

  R6 class `Grouping` object or its input

- `shape`:

  R6 class `Grouping` object or its input

- `error`:

  mapping error bars around scatter points

- `uncertainty`:

  **\[deprecated\]** uncertainty were replaced by `error` argument.
  Mapping error bars around scatter points.

- `mdv`:

  mapping missing dependent variable

- `data`:

  data.frame to map used by `.smartMapping`

- `lloq`:

  mapping lloq lines

#### Returns

A new `ObservedDataMapping` object

------------------------------------------------------------------------

### Method `checkMapData()`

Check that `data` variables include map variables

#### Usage

    ObservedDataMapping$checkMapData(data, metaData = NULL)

#### Arguments

- `data`:

  data.frame to check

- `metaData`:

  list containing information on `data`

#### Returns

A data.frame with map and `defaultAes` variables. Dummy variable
`defaultAes` is necessary to allow further modification of plots.

------------------------------------------------------------------------

### Method `requireDualAxis()`

Assess if `data` require a dual axis plot

#### Usage

    ObservedDataMapping$requireDualAxis(data)

#### Arguments

- `data`:

  data.frame to check

#### Returns

A logical

------------------------------------------------------------------------

### Method `getLeftAxis()`

Render NA values for all right axis data

#### Usage

    ObservedDataMapping$getLeftAxis(data)

#### Arguments

- `data`:

  A data.frame

#### Returns

A data.frame to be plotted in left axis

------------------------------------------------------------------------

### Method `getRightAxis()`

Render NA values for all left axis data

#### Usage

    ObservedDataMapping$getRightAxis(data)

#### Arguments

- `data`:

  A data.frame

#### Returns

A data.frame to be plotted in right axis

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ObservedDataMapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
