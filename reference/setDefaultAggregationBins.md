# setDefaultAggregationBins

Set default aggregation bins of tlf environment

## Usage

``` r
setDefaultAggregationBins(bins = NULL)
```

## Arguments

- bins:

  Number of bins if value, edges if vector or binning function if
  function

## Examples

``` r
# Set default number of bins
plotHistogram(x = rnorm(1000))
#> Warning: Ignoring unknown parameters: `size`


setDefaultAggregationBins(21)
plotHistogram(x = rnorm(1000))
#> Warning: Ignoring unknown parameters: `size`

```
