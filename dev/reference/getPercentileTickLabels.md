# getPercentileTickLabels

Get ticklabels expressions for percentiles of normal distribution scale
plots

## Usage

``` r
getPercentileTickLabels(ticks)
```

## Arguments

- ticks:

  numeric values of the ticks

## Value

Expressions to use in `ticklabels` input parameter of `setXAxis` and
`setYAxis` functions

## Examples

``` r
ticks <- rnorm(5)
getPercentileTickLabels(ticks)
#> [1] "57.69%"  "34.9%"   "52.651%" "29.645%" "20.273%"

# Get percentile of normal distribution
ticks <- qnorm(seq(1, 9) / 10)
getPercentileTickLabels(ticks)
#> [1] "10%" "20%" "30%" "40%" "50%" "60%" "70%" "80%" "90%"
```
