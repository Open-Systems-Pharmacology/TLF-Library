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
#> A 500      0.1793389       0.4720784       1.0570239        1.985663
#> B 500      0.1774636       0.4933805       0.8893214        2.065655
#>   95th percentile     mean standard deviation  geo mean geo standard deviation
#> A        4.677547 1.583360           1.863102 0.9726612               2.758563
#> B        6.066301 1.695948           2.053495 0.9734652               2.923722
```
