# plotQQ

Producing Histograms

## Usage

``` r
plotQQ(
  data = NULL,
  metaData = NULL,
  y = NULL,
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

- y:

  Numeric values to plot along the `y` axis. Only used instead of `data`
  if `data` is `NULL`.

- dataMapping:

  A `QQDataMapping` object mapping `y` and aesthetic groups to their
  variable names of `data`.

- plotConfiguration:

  An optional `QQPlotConfiguration` object defining labels, grid,
  background and watermark.

- plotObject:

  An optional `ggplot` object on which to add the plot layer

## Value

A `ggplot` object

## See also

Other molecule plots:
[`plotBoxWhisker()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotBoxWhisker.md),
[`plotCumulativeTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotCumulativeTimeProfile.md),
[`plotDDIRatio()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotDDIRatio.md),
[`plotGrid()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotGrid.md),
[`plotHistogram()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotHistogram.md),
[`plotObsVsPred()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotObsVsPred.md),
[`plotObservedTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotObservedTimeProfile.md),
[`plotPKRatio()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotPKRatio.md),
[`plotPieChart()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotPieChart.md),
[`plotResVsPred()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotResVsPred.md),
[`plotResVsTime()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotResVsTime.md),
[`plotSimulatedTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotSimulatedTimeProfile.md),
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotTimeProfile.md),
[`plotTornado()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotTornado.md)

## Examples

``` r
# Produce QQ plot of normally distributed data
plotQQ(y = rnorm(100))


# Produce QQ plot of normally distributed data split by group
qqData <- data.frame(
  residuals = c(rnorm(100), rnorm(100)),
  groups = c(rep("Group A", 100), rep("Group B", 100))
)
plotQQ(
  data = qqData,
  dataMapping = QQDataMapping$new(y = "residuals", group = "groups")
)

```
