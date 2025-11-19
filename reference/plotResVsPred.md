# plotResVsPred

Producing residuals vs predicted plots

## Usage

``` r
plotResVsPred(
  data,
  metaData = NULL,
  dataMapping = NULL,
  plotConfiguration = NULL,
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

  A `ResVsPredDataMapping` object mapping `x`, `y` and aesthetic groups
  to their variable names of `data`.

- plotConfiguration:

  An optional `ResVsPredConfiguration` object defining labels, grid,
  background and watermark.

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
[`plotBoxWhisker()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotBoxWhisker.md),
[`plotCumulativeTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotCumulativeTimeProfile.md),
[`plotDDIRatio()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotDDIRatio.md),
[`plotGrid()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotGrid.md),
[`plotHistogram()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotHistogram.md),
[`plotObsVsPred()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotObsVsPred.md),
[`plotObservedTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotObservedTimeProfile.md),
[`plotPKRatio()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotPKRatio.md),
[`plotPieChart()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotPieChart.md),
[`plotQQ()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotQQ.md),
[`plotResVsTime()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotResVsTime.md),
[`plotSimulatedTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotSimulatedTimeProfile.md),
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotTimeProfile.md),
[`plotTornado()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotTornado.md)

## Examples

``` r
# Produce Obs vs Pred plot
resVsPredData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))

plotResVsPred(data = resVsPredData, dataMapping = ResVsPredDataMapping$new(x = "x", y = "y"))


# Produce Res vs Pred plot with linear regression
plotResVsPred(
  data = resVsPredData,
  dataMapping = ResVsPredDataMapping$new(x = "x", y = "y"),
  smoother = "lm"
)
```
