# getBoxWhiskerMeasure

Get a summary table of Box Whisker percentiles

## Usage

``` r
getBoxWhiskerMeasure(
  data,
  dataMapping = NULL,
  y = NULL,
  group = NULL,
  quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95)
)
```

## Arguments

- data:

  A data.frame to use for plot.

- dataMapping:

  A `BoxWhiskerDataMapping` object mapping `x`, `y` and aesthetic groups
  to their variable names of `data`.

- y:

  Name of `y` variable in `data`.

- group:

  Name of grouping variable in `data`.

- quantiles:

  Numeric values between 0 and 1 defining the quantiles to summarize

## Value

A data.frame of summary statistics

## Examples

``` r
# Get box-and-whisker plots of log-normal distributed data
boxData <- data.frame(x = c(rep("A", 500), rep("B", 500)), y = rlnorm(1000))

getBoxWhiskerMeasure(data = boxData, dataMapping = BoxWhiskerDataMapping$new(x = "x", y = "y"))
#>     N 5th percentile 25th percentile 50th percentile 75th percentile
#> A 500      0.2187810       0.5043485       0.9663398        1.895255
#> B 500      0.2179715       0.4960075       0.9108794        1.949978
#>   95th percentile     mean standard deviation  geo mean geo standard deviation
#> A        4.788241 1.548360           1.837110 0.9781572               2.609074
#> B        4.666546 1.572032           1.781757 0.9872647               2.617103
```
