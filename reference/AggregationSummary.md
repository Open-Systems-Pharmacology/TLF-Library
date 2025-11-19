# AggregationSummary

R6 class to split a data.frame data into subsets defined by unique
combinations of elements in the columns `xColumnNames` and
`groupingColumnNames`. Applies functions defined in
`aggregationFunctionsVector` to column `yColumnNames.` Returns a list of
data.frame, one data.frame for each function listed in
`aggregationFunctionsVector`. Each data.frame in list element is named
after the function's corresponding string in `aggregationFunctionNames`.
The summary statistic column name in each data.frame is the same as the
name of the data.frame in the returned list.

## Public fields

- `data`:

  data.frame

- `metaData`:

  list of information on `data`

- `xColumnNames`:

  character names of grouping variables

- `groupingColumnNames`:

  character names of grouping variables

- `yColumnNames`:

  character names of dependent variables (that are grouped)

- `aggregationInputsVector`:

  list of R6 class `AggregationInput` objects

- `aggregationFunctionsVector`:

  list of functions to use for aggregation

- `aggregationFunctionNames`:

  vector of function names that will be used as variable name of the
  aggregation

- `aggregationUnitsVector`:

  character vector of units of aggregation output

- `aggregationDimensionsVector`:

  character vector of dimensions of aggregation output

- `dfHelper`:

  data.frame of aggregated values

- `metaDataHelper`:

  list of information on `dfHelper`

## Methods

### Public methods

- [`AggregationSummary$new()`](#method-AggregationSummary-new)

- [`AggregationSummary$applyAggregationFunctions()`](#method-AggregationSummary-applyAggregationFunctions)

- [`AggregationSummary$generateAggregatedValues()`](#method-AggregationSummary-generateAggregatedValues)

- [`AggregationSummary$clone()`](#method-AggregationSummary-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `AggregationSummary` object

#### Usage

    AggregationSummary$new(
      data,
      metaData = NULL,
      xColumnNames = NULL,
      groupingColumnNames = NULL,
      yColumnNames = NULL,
      aggregationInputsVector = NULL,
      aggregationFunctionsVector = NULL,
      aggregationFunctionNames = NULL,
      aggregationUnitsVector = NULL,
      aggregationDimensionsVector = NULL
    )

#### Arguments

- `data`:

  data.frame

- `metaData`:

  list of information on `data`

- `xColumnNames`:

  character names of grouping variables

- `groupingColumnNames`:

  character names of grouping variables

- `yColumnNames`:

  character names of dependent variables (that are grouped)

- `aggregationInputsVector`:

  list of R6 class `AggregationInput` objects

- `aggregationFunctionsVector`:

  list of functions to use for aggregation

- `aggregationFunctionNames`:

  vector of function names that will be used as variable name of the
  aggregation

- `aggregationUnitsVector`:

  character vector of units of aggregation output

- `aggregationDimensionsVector`:

  character vector of dimensions of aggregation output

#### Returns

A new `AggregationSummary` object

------------------------------------------------------------------------

### Method `applyAggregationFunctions()`

Apply aggregation functions on `x`

#### Usage

    AggregationSummary$applyAggregationFunctions(x)

#### Arguments

- `x`:

  numeric vector

#### Returns

A list or vector of aggregated values

------------------------------------------------------------------------

### Method `generateAggregatedValues()`

Generate aggregated values

#### Usage

    AggregationSummary$generateAggregatedValues()

#### Returns

A list or vector of aggregated values

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AggregationSummary$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
