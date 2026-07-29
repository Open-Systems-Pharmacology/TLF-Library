# plotPKRatio

Producing PK Ratio plots

## Usage

``` r
plotPKRatio(
  data,
  metaData = NULL,
  dataMapping = NULL,
  plotConfiguration = NULL,
  foldDistance = NULL,
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

  A `PKRatioDataMapping` object mapping `x`, `y` and aesthetic groups to
  their variable names of `data`.

- plotConfiguration:

  An optional `PKRatioPlotConfiguration` object defining labels, grid,
  background and watermark.

- foldDistance:

  Numeric values of fold distance lines to display in log plots. This
  argument is internally translated into `lines` field of `dataMapping`.
  **Caution**: this argument is meant for log scaled plots and since
  fold distance is a ratio it is expected positive. In particular, line
  of identity corresponds to a `foldDistance` of `1`.

- plotObject:

  An optional `ggplot` object on which to add the plot layer

## Value

A `ggplot` object

## References

For examples, see:
<https://www.open-systems-pharmacology.org/TLF-Library/articles/pk-ratio-vignette.html>

## See also

Other molecule plots:
[`plotBoxWhisker()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotBoxWhisker.md),
[`plotCumulativeTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotCumulativeTimeProfile.md),
[`plotDDIRatio()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotDDIRatio.md),
[`plotGrid()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotGrid.md),
[`plotHistogram()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotHistogram.md),
[`plotObsVsPred()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotObsVsPred.md),
[`plotObservedTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotObservedTimeProfile.md),
[`plotPieChart()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotPieChart.md),
[`plotQQ()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotQQ.md),
[`plotResVsPred()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotResVsPred.md),
[`plotResVsTime()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotResVsTime.md),
[`plotSimulatedTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotSimulatedTimeProfile.md),
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotTimeProfile.md),
[`plotTornado()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotTornado.md)

## Examples

``` r
# Produce PK Ratio plot
pkData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))

plotPKRatio(data = pkData, dataMapping = PKRatioDataMapping$new(x = "x", y = "y"))


# Produce PK Ratio plot with user-defined horizontal lines
plotPKRatio(
  data = pkData,
  dataMapping = PKRatioDataMapping$new(x = "x", y = "y"),
  foldDistance = c(1, 10)
)

```
