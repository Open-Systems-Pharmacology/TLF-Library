# plotCumulativeTimeProfile

Producing Cumulative Time Profile plots

## Usage

``` r
plotCumulativeTimeProfile(
  data = NULL,
  metaData = NULL,
  dataMapping = NULL,
  colorPalette = NULL,
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

  A `CumulativeTimeProfileDataMapping` object mapping `x` and aesthetic
  groups to their variable names of `data`.

- colorPalette:

  Optional character values defining a `ggplot2` colorPalette (e.g.
  `"Set1"` or `"Spectral"`)

- plotConfiguration:

  An optional `CumulativeTimeProfilePlotConfiguration` object defining
  labels, grid, background and watermark.

- plotObject:

  An optional `ggplot` object on which to add the plot layer

## Value

A `ggplot` object

## See also

Other molecule plots:
[`plotBoxWhisker()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotBoxWhisker.md),
[`plotDDIRatio()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotDDIRatio.md),
[`plotGrid()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotGrid.md),
[`plotHistogram()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotHistogram.md),
[`plotObsVsPred()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotObsVsPred.md),
[`plotObservedTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotObservedTimeProfile.md),
[`plotPKRatio()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotPKRatio.md),
[`plotPieChart()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotPieChart.md),
[`plotQQ()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotQQ.md),
[`plotResVsPred()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotResVsPred.md),
[`plotResVsTime()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotResVsTime.md),
[`plotSimulatedTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotSimulatedTimeProfile.md),
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotTimeProfile.md),
[`plotTornado()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotTornado.md)

## Examples

``` r

# Define data to be plotted as cumulative time profile
time <- seq(1, 10)
data <- data.frame(
  x = rep(time),
  y = c(exp(-time / 10), 1 - exp(-time / 10)),
  legend = rep(c("decreasing area", "increasing area"), each = 10)
)

# Produce a Cumulative Time Profile plot
plotCumulativeTimeProfile(
  data = data,
  dataMapping = CumulativeTimeProfileDataMapping$new(x = "x", y = "y", fill = "legend")
)


# Produce a Cumulative Time Profile plot with a ggplot2 color palette
plotCumulativeTimeProfile(
  data = data,
  dataMapping = CumulativeTimeProfileDataMapping$new(
    x = "x",
    y = "y",
    fill = "legend"
  ),
  colorPalette = "Set1"
)

```
