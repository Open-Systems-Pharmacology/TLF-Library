# setBackground

Set background properties of a `ggplot` object

## Usage

``` r
setBackground(
  plotObject,
  fill = NULL,
  color = NULL,
  linetype = NULL,
  size = NULL
)
```

## Arguments

- plotObject:

  A `ggplot` object

- fill:

  Optional character values defining the color of the background. See
  [`grDevices::colors()`](https://rdrr.io/r/grDevices/colors.html) to
  get names of colors

- color:

  Optional character values defining the color of the background frame.
  See [`grDevices::colors()`](https://rdrr.io/r/grDevices/colors.html)
  to get names of colors

- linetype:

  Optional character values defining the linetype of the background
  frame. See enum `Linetypes` to get names of linetype.

- size:

  Optional numeric values defining the size of the background frame.

## Value

A `ggplot` object

## Examples

``` r
# Set background of a scatter plot
p <- addScatter(x = c(1, 2, 1, 2, 3), y = c(5, 0, 2, 3, 4))

setBackground(p, fill = "yellowgreen", color = "red", linetype = "dotted")

```
