# getPiTickLabels

Get ticklabels expressions for plots with values as ratios of Pi

## Usage

``` r
getPiTickLabels(ticks)
```

## Arguments

- ticks:

  numeric values of the ticks

## Value

Expressions to use in `ticklabels` input parameter of `setXAxis` and
`setYAxis` functions

## Examples

``` r
ticks <- seq(0, 2 * pi, pi / 2)
getPiTickLabels(ticks)
#> [1] "0"       " + π/2"  "π"       "π + π/2" "2π"     
```
