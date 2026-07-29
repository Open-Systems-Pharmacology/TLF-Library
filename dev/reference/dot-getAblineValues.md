# .getAblineValues

Apply log10 transformation to lines because plot is log scaled by
default and geom_abline requires the log transformed values in input of
intercept Get list of values to provide to `lines` argument of
`dataMapping` objects. The `lines` are internally used as argument of
`geom_hline` and `geom_abline` from `ggplot2`

## Usage

``` r
.getAblineValues(values, scale)
```

## Arguments

- values:

  Numeric values to be transformed if scale is log

- scale:

  Scale of axis. Use enum `Scaling` to access names of scales.

## Value

Numeric values
