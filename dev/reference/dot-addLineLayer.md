# .addLineLayer

Add line layer of a molecule plot

## Usage

``` r
.addLineLayer(plotObject, type, value, position)
```

## Arguments

- plotObject:

  A `ggplot` object

- type:

  one of "horizontal", "vertical" or "diagonal" Note that for
  "diagonal", geom_abline is used. `value` of intercept is taken as is
  for linear scale but corresponds to the log of `value` for log scale.
  For instance, intercept = c(-1, 0, 1) with log scale actually means
  that the line will go through c(0.1, 1, 10) because c(-1, 0, 1) =
  log10(c(0.1, 1, 10)).

- value:

  value of xintercept or yintercept

- position:

  line position for aesthetic properties

## Value

A `ggplot` object
