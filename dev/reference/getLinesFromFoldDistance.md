# getLinesFromFoldDistance

Get list of values to provide to `lines` argument of `dataMapping`
objects. The `lines` are internally used as argument of `geom_hline` and
`geom_abline` from `ggplot2`

## Usage

``` r
getLinesFromFoldDistance(foldDistance)
```

## Arguments

- foldDistance:

  Numeric values **Caution**: this argument is meant for log scaled
  plots and since fold distance is a ratio it is expected positive. In
  particular, line of identity corresponds to a `foldDistance` of `1`.

## Value

A list of numeric values

## Examples

``` r

# Get lines for identity and 2-fold distance
getLinesFromFoldDistance(c(1, 2))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2.0 0.5
#> 

# Create dataMapping with lines identity and 2-fold distance
dataMapping <- ObsVsPredDataMapping$new(
  x = "predicted",
  y = "observed",
  lines = getLinesFromFoldDistance(c(1, 2))
)
```
