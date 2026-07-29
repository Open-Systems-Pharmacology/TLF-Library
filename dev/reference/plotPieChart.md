# plotPieChart

Producing a Pie Chart

## Usage

``` r
plotPieChart(
  data = NULL,
  metaData = NULL,
  dataMapping = NULL,
  colorPalette = NULL,
  start = NULL,
  clockwiseDirection = NULL,
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

- dataMapping:

  A `PieChartDataMapping` object mapping `x` and `fill` aesthetic groups
  to their variable names of `data`. Values mapped to `y` variable will
  be displayed as text within the pie chart

- colorPalette:

  color palette property from `ggplot2`

- start:

  Offset of starting point from 12 o'clock in radians. Offset is applied
  clockwise or anticlockwise depending on value of direction

- clockwiseDirection:

  logical defining if values are displayed in clockwise order

- plotConfiguration:

  An optional `PieChartPlotConfiguration` object defining labels, grid,
  background and watermark.

- plotObject:

  An optional `ggplot` object on which to add the plot layer

## Value

A `ggplot` object

## See also

Other molecule plots:
[`plotBoxWhisker()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotBoxWhisker.md),
[`plotCumulativeTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotCumulativeTimeProfile.md),
[`plotDDIRatio()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotDDIRatio.md),
[`plotGrid()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotGrid.md),
[`plotHistogram()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotHistogram.md),
[`plotObsVsPred()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotObsVsPred.md),
[`plotObservedTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotObservedTimeProfile.md),
[`plotPKRatio()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotPKRatio.md),
[`plotQQ()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotQQ.md),
[`plotResVsPred()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotResVsPred.md),
[`plotResVsTime()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotResVsTime.md),
[`plotSimulatedTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotSimulatedTimeProfile.md),
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotTimeProfile.md),
[`plotTornado()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotTornado.md)

## Examples

``` r
# Data for the pie chart
values <- runif(5)
data <- data.frame(
  values = values,
  text = paste0(round(100 * values / sum(values)), "%"),
  legend = letters[1:5]
)

# Plot pie chart with its legend
plotPieChart(
  data = data,
  dataMapping = PieChartDataMapping$new(x = "values", fill = "legend")
)


# Plot pie chart with text within pie
plotPieChart(
  data = data,
  dataMapping = PieChartDataMapping$new(x = "values", y = "text", fill = "legend")
)


# Reverse direction of pie chart
plotPieChart(
  data = data,
  dataMapping = PieChartDataMapping$new(x = "values", y = "text", fill = "legend"),
  clockwiseDirection = FALSE
)


# Start first slice of pie at 90 degrees
plotPieChart(
  data = data,
  dataMapping = PieChartDataMapping$new(x = "values", y = "text", fill = "legend"),
  start = pi / 2
)


# Leverages ggplot color palettes
plotPieChart(
  data = data,
  dataMapping = PieChartDataMapping$new(x = "values", y = "text", fill = "legend"),
  colorPalette = ColorPalettes$Set1
)

```
