# AggregationInput

R6 class to be used to construct inputs to the AggregationSummary class

## Public fields

- `aggregationFunction`:

  list of functions to use for aggregation

- `aggregationFunctionName`:

  vector of function names that will be used as variable name of the
  aggregation

- `aggregationUnit`:

  unit of aggregation output

- `aggregationDimension`:

  dimension of aggregation output

## Methods

### Public methods

- [`AggregationInput$new()`](#method-AggregationInput-new)

- [`AggregationInput$clone()`](#method-AggregationInput-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `AggregationInput` object

#### Usage

    AggregationInput$new(
      aggregationFunction = NULL,
      aggregationFunctionName = NULL,
      aggregationUnit = NULL,
      aggregationDimension = NULL
    )

#### Arguments

- `aggregationFunction`:

  list of functions to use for aggregation

- `aggregationFunctionName`:

  vector of function names that will be used as variable name of the
  aggregation

- `aggregationUnit`:

  unit of aggregation output

- `aggregationDimension`:

  dimension of aggregation output

#### Returns

A new `AggregationInput` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AggregationInput$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
