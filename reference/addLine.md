# addLine

Add a line layer to a `ggplot` object.

## Usage

``` r
addLine(
  data = NULL,
  metaData = NULL,
  x = NULL,
  y = NULL,
  caption = NULL,
  color = NULL,
  shape = NULL,
  size = NULL,
  linetype = NULL,
  dataMapping = NULL,
  plotConfiguration = NULL,
  plotObject = NULL
)
```

## Arguments

- data:

  A data.frame to use for plot.

- metaData:

  A named list of information about `data` such as the `dimension` and
  `unit` of its variables.

- x:

  Numeric values to plot along the `x` axis. Only used instead of `data`
  if `data` is `NULL`.

- y:

  Numeric values to plot along the `y` axis. Only used instead of `data`
  if `data` is `NULL`.

- caption:

  Optional character values defining the legend captions of the plot.

- color:

  Optional character values defining the colors of the plot layer. See
  [`grDevices::colors()`](https://rdrr.io/r/grDevices/colors.html) to
  get names of colors

- shape:

  Optional character values defining the shapes/symbols of the plot
  layer. See enum `Shapes` to get names of shapes.

- size:

  Optional numeric values defining the size of the plot layer.

- linetype:

  Optional character values defining the linetype of the plot layer. See
  enum `Linetypes` to get names of linetype.

- dataMapping:

  A `XYGDataMapping` object mapping `x`, `y` and aesthetic groups to
  their variable names of `data`.

- plotConfiguration:

  An optional `PlotConfiguration` object defining labels, grid,
  background and watermark.

- plotObject:

  An optional `ggplot` object on which to add the plot layer

## Value

A `ggplot` object

## References

For examples, see:
<https://www.open-systems-pharmacology.org/TLF-Library/articles/atom-plots.html>

## See also

Other atom plots:
[`addErrorbar()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/addErrorbar.md),
[`addRibbon()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/addRibbon.md),
[`addScatter()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/addScatter.md),
[`initializePlot()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/initializePlot.md)

## Examples

``` r
# Add line using x and y
addLine(x = c(1, 2, 1, 2, 3), y = c(5, 0, 2, 3, 4))


# Add line using a data.frame
time <- seq(0, 30, 0.1)
lineData <- data.frame(x = time, y = cos(time))

addLine(
  data = lineData,
  dataMapping = XYGDataMapping$new(x = "x", y = "y")
)


# Or for simple cases a smart mapping will get directly x and y from data
addLine(data = lineData)


# Add a line with caption
addLine(data = lineData, caption = "My line plot")


# Add a line with specific properties
addLine(
  data = lineData,
  color = "blue", linetype = "longdash", size = 0.5, caption = "My data"
)


# Add a line with specific properties
p <- addLine(
  data = lineData,
  color = "blue", linetype = "longdash", size = 0.5, caption = "My data"
)
addLine(
  x = c(0, 1), y = c(1, 0),
  color = "red", linetype = "solid", size = 1,
  plotObject = p
)

```
