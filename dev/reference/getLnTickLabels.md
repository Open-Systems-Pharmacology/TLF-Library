# getLnTickLabels

Get ticklabels expressions for ln scale plots

## Usage

``` r
getLnTickLabels(ticks)
```

## Arguments

- ticks:

  numeric values of the ticks

## Value

Expressions to use in `ticklabels` input parameter of `setXAxis` and
`setYAxis` functions

## Examples

``` r
ticks <- exp(c(1, 5, 10, 50, 100, 500))
getLnTickLabels(ticks)
#> expression(e^1, e^5, e^10, e^50, e^100, e^500)
```
