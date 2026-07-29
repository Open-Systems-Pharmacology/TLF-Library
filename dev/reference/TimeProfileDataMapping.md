# TimeProfileDataMapping

R6 class defining the configuration of a `ggplot` object for time
profile plot

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
[`TornadoDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/TornadoDataMapping.md),
[`XYDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XYDataMapping.md),
[`XYGDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XYGDataMapping.md)

## Super classes

[`tlf::XYDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XYDataMapping.md)
-\>
[`tlf::XYGDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/XYGDataMapping.md)
-\>
[`tlf::RangeDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/RangeDataMapping.md)
-\> `TimeProfileDataMapping`

## Public fields

- `y2Axis`:

  Name of y2Axis variable to map

## Methods

### Public methods

- [`TimeProfileDataMapping$new()`](#method-TimeProfileDataMapping-new)

- [`TimeProfileDataMapping$checkMapData()`](#method-TimeProfileDataMapping-checkMapData)

- [`TimeProfileDataMapping$requireDualAxis()`](#method-TimeProfileDataMapping-requireDualAxis)

- [`TimeProfileDataMapping$getLeftAxis()`](#method-TimeProfileDataMapping-getLeftAxis)

- [`TimeProfileDataMapping$getRightAxis()`](#method-TimeProfileDataMapping-getRightAxis)

- [`TimeProfileDataMapping$clone()`](#method-TimeProfileDataMapping-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `TimeProfileDataMapping` object

#### Usage

    TimeProfileDataMapping$new(
      x = NULL,
      y = NULL,
      ymin = NULL,
      ymax = NULL,
      group = NULL,
      y2Axis = NULL,
      color = NULL,
      fill = NULL,
      linetype = NULL,
      data = NULL
    )

#### Arguments

- `x`:

  Name of x variable to map

- `y`:

  Name of y variable to map

- `ymin`:

  Name of ymin variable to map

- `ymax`:

  Name of ymax variable to map

- `group`:

  R6 class `Grouping` object or its input

- `y2Axis`:

  Name of y2Axis variable to map

- `color`:

  R6 class `Grouping` object or its input

- `fill`:

  R6 class `Grouping` object or its input

- `linetype`:

  R6 class `Grouping` object or its input

- `data`:

  data.frame to map used by `.smartMapping`

#### Returns

A new `RangeDataMapping` object

------------------------------------------------------------------------

### Method `checkMapData()`

Check that `data` variables include map variables

#### Usage

    TimeProfileDataMapping$checkMapData(data, metaData = NULL)

#### Arguments

- `data`:

  data.frame to check

- `metaData`:

  list containing information on `data`

#### Returns

A data.frame with map and `legendLabels` variables. Dummy variable
`legendLabels` is necessary to allow further modification of plots.

------------------------------------------------------------------------

### Method `requireDualAxis()`

Assess if `data` require a dual axis plot

#### Usage

    TimeProfileDataMapping$requireDualAxis(data)

#### Arguments

- `data`:

  data.frame to check

#### Returns

A logical

------------------------------------------------------------------------

### Method `getLeftAxis()`

Render NA values for all right axis data

#### Usage

    TimeProfileDataMapping$getLeftAxis(data)

#### Arguments

- `data`:

  A data.frame

#### Returns

A data.frame to be plotted in left axis

------------------------------------------------------------------------

### Method `getRightAxis()`

Render NA values for all left axis data

#### Usage

    TimeProfileDataMapping$getRightAxis(data)

#### Arguments

- `data`:

  A data.frame

#### Returns

A data.frame to be plotted in right axis

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TimeProfileDataMapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
