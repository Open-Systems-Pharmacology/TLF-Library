# getLogTickLabels

Get ticklabels expressions for log scale plots

## Usage

``` r
getLogTickLabels(ticks)
```

## Arguments

- ticks:

  numeric values of the ticks

## Value

Expressions to use in `ticklabels` input parameter of `setXAxis` and
`setYAxis` functions

## Examples

``` r
ticks <- c(1, 5, 10, 50, 100, 500)
getLogTickLabels(ticks)
#> expression(10^0, 5 %*% 10^0, 10^1, 5 %*% 10^1, 10^2, 5 %*% 10^2)
```
