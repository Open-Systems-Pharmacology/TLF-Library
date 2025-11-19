# ObsVsPredDataMapping

Class for mapping variables in observations vs predictions plot

## See also

Other DataMapping classes:
[`BoxWhiskerDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/BoxWhiskerDataMapping.md),
[`CumulativeTimeProfileDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/CumulativeTimeProfileDataMapping.md),
[`DDIRatioDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/DDIRatioDataMapping.md),
[`GroupMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/GroupMapping.md),
[`Grouping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Grouping.md),
[`HistogramDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/HistogramDataMapping.md),
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

## Super classes

[`tlf::XYDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/XYDataMapping.md)
-\>
[`tlf::XYGDataMapping`](https://www.open-systems-pharmacology.org/TLF-Library/reference/XYGDataMapping.md)
-\> `ObsVsPredDataMapping`

## Public fields

- `lines`:

  list of ratio limits to plot as horizontal lines

- `xmin`:

  mapping of upper value of error bars around scatter points

- `xmax`:

  mapping of lower value of error bars around scatter points

- `smoother`:

  regression function name

- `lloq`:

  mapping lloq lines

## Methods

### Public methods

- [`ObsVsPredDataMapping$new()`](#method-ObsVsPredDataMapping-new)

- [`ObsVsPredDataMapping$checkMapData()`](#method-ObsVsPredDataMapping-checkMapData)

- [`ObsVsPredDataMapping$clone()`](#method-ObsVsPredDataMapping-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `ObsVsPredDataMapping` object

#### Usage

    ObsVsPredDataMapping$new(
      x = NULL,
      y = NULL,
      xmin = NULL,
      xmax = NULL,
      lines = DefaultDataMappingValues$obsVsPred,
      smoother = NULL,
      lloq = NULL,
      ...
    )

#### Arguments

- `x`:

  Name of x variable to map

- `y`:

  Name of y variable to map

- `xmin`:

  mapping of upper value of error bars around scatter points

- `xmax`:

  mapping of lower value of error bars around scatter points

- `lines`:

  list of lines to plot

- `smoother`:

  smoother function or parameter

- `lloq`:

  mapping lloq lines To map a loess smoother to the plot, use
  `smoother`="loess"

- `...`:

  parameters inherited from `XYGDataMapping`

#### Returns

A new `ObsVsPredDataMapping` object

------------------------------------------------------------------------

### Method `checkMapData()`

Check that `data` variables include map variables

#### Usage

    ObsVsPredDataMapping$checkMapData(data, metaData = NULL)

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

    ObsVsPredDataMapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
