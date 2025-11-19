# plotDDIRatio

Producing DDI Ratio plots

## Usage

``` r
plotDDIRatio(
  data,
  metaData = NULL,
  dataMapping = NULL,
  plotConfiguration = NULL,
  residualsVsObserved = NULL,
  foldDistance = NULL,
  deltaGuest = NULL,
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

  A `DDIRatioDataMapping` object mapping `x`, `y` and aesthetic groups
  to their variable names of `data`.

- plotConfiguration:

  An optional `DDIRatioPlotConfiguration` object defining labels, grid,
  background and watermark.

- residualsVsObserved:

  Optional logical value defining if DDI Ratio plot is drawn as
  residuals vs observed, instead of predicted vs observed.

- foldDistance:

  Numeric values of fold distance lines to display in log plots. This
  argument is internally translated into `lines` field of `dataMapping`.
  **Caution**: this argument is meant for log scaled plots and since
  fold distance is a ratio it is expected positive. In particular, line
  of identity corresponds to a `foldDistance` of `1`.

- deltaGuest:

  Numeric value parameter of Guest function

- plotObject:

  An optional `ggplot` object on which to add the plot layer

## Value

A `ggplot` object

## References

For examples, see:
<https://www.open-systems-pharmacology.org/TLF-Library/articles/pk-ratio-vignette.html>

## See also

Other molecule plots:
[`plotBoxWhisker()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotBoxWhisker.md),
[`plotCumulativeTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotCumulativeTimeProfile.md),
[`plotGrid()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotGrid.md),
[`plotHistogram()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotHistogram.md),
[`plotObsVsPred()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotObsVsPred.md),
[`plotObservedTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotObservedTimeProfile.md),
[`plotPKRatio()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotPKRatio.md),
[`plotPieChart()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotPieChart.md),
[`plotQQ()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotQQ.md),
[`plotResVsPred()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotResVsPred.md),
[`plotResVsTime()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotResVsTime.md),
[`plotSimulatedTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotSimulatedTimeProfile.md),
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotTimeProfile.md),
[`plotTornado()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotTornado.md)

## Examples

``` r
# Produce DDI Ratio plot
ddiData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))

plotDDIRatio(data = ddiData, dataMapping = DDIRatioDataMapping$new(x = "x", y = "y"))


# Produce DDI Ratio plot with user-defined horizontal lines
plotDDIRatio(
  data = ddiData,
  dataMapping = DDIRatioDataMapping$new(x = "x", y = "y"),
  foldDistance = c(1, 10),
  deltaGuest = 1.25,
  residualsVsObserved = TRUE
)

```
