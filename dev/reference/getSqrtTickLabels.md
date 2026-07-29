# getSqrtTickLabels

Get ticklabels expressions for sqrt scale plots

## Usage

``` r
getSqrtTickLabels(ticks)
```

## Arguments

- ticks:

  numeric values of the ticks

## Value

Expressions to use in `ticklabels` input parameter of `setXAxis` and
`setYAxis` functions

## Examples

``` r
ticks <- sqrt(c(1, 5, 10, 50, 100, 500))
getSqrtTickLabels(ticks)
#> expression(sqrt(1), sqrt(5), sqrt(10), sqrt(50), sqrt(100), sqrt(500))
```
