# setGrid

Set x and y grid properties of a `ggplot` object

## Usage

``` r
setGrid(plotObject, color = NULL, linetype = NULL, size = NULL)
```

## Arguments

- plotObject:

  A `ggplot` object

- color:

  Optional character values defining the color of the grid. See
  [`grDevices::colors()`](https://rdrr.io/r/grDevices/colors.html) to
  get names of colors

- linetype:

  Optional character values defining the linetype of the grid. See enum
  `Linetypes` to get names of linetype.

- size:

  Optional numeric values defining the size of the grid.

## Value

A `ggplot` object

## Examples

``` r
# Set grid of a scatter plot
p <- addScatter(x = c(1, 2, 1, 2, 3), y = c(5, 0, 2, 3, 4))

setGrid(p, color = "red", linetype = "dotted")

```
