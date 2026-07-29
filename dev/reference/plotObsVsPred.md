# plotObsVsPred

Producing observed vs predicted plots

## Usage

``` r
plotObsVsPred(
  data,
  metaData = NULL,
  dataMapping = NULL,
  plotConfiguration = NULL,
  foldDistance = NULL,
  smoother = NULL,
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

  A `ObsVsPredDataMapping` object mapping `x`, `y` and aesthetic groups
  to their variable names of `data`.

- plotConfiguration:

  An optional `ObsVsPredConfiguration` object defining labels, grid,
  background and watermark.

- foldDistance:

  Numeric values of fold distance lines to display in log plots. This
  argument is internally translated into `lines` field of `dataMapping`.
  **Caution**: this argument is meant for log scaled plots and since
  fold distance is a ratio it is expected positive. In particular, line
  of identity corresponds to a `foldDistance` of `1`.

- smoother:

  Optional name of smoother function:

  - `"loess"` for loess regression

  - `"lm"` for linear regression

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
# Produce Obs vs Pred plot
obsVsPredData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))

plotObsVsPred(data = obsVsPredData, dataMapping = ObsVsPredDataMapping$new(x = "x", y = "y"))


# Produce Obs vs Pred plot with linear regression
plotObsVsPred(
  data = obsVsPredData,
  dataMapping = ObsVsPredDataMapping$new(x = "x", y = "y"),
  smoother = "lm"
)
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the tlf package.
#>   Please report the issue at
#>   <https://github.com/open-systems-pharmacology/tlf-library/issues>.


# Produce Obs vs Pred plot with user-defined fold distance lines
plotObsVsPred(
  data = obsVsPredData,
  dataMapping = ObsVsPredDataMapping$new(x = "x", y = "y"),
  plotConfiguration = ObsVsPredPlotConfiguration$new(
    xScale = Scaling$log, xAxisLimits = c(0.05, 50),
    yScale = Scaling$log, yAxisLimits = c(0.05, 50)
  ),
  foldDistance = c(1, 10)
)

```
