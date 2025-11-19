# getGreekTickLabels

Get ticklabels expressions for discrete scale plots with greek letters

## Usage

``` r
getGreekTickLabels(ticks)
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
getGreekTickLabels(ticks)
#> [1] "α" "ε" "κ" "Ϣ" "Д" "֤" 
```
