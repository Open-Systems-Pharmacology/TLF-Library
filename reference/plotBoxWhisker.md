# plotBoxWhisker

Producing box-and-whisker plots

## Usage

``` r
plotBoxWhisker(
  data,
  metaData = NULL,
  outliers = NULL,
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

- outliers:

  Logical defining if outliers should be included in boxplot

- dataMapping:

  A `BoxWhiskerDataMapping` object mapping `x`, `y` and aesthetic groups
  to their variable names of `data`.

- plotConfiguration:

  An optional `BoxWhiskerConfiguration` object defining labels, grid,
  background and watermark.

- plotObject:

  An optional `ggplot` object on which to add the plot layer

## Value

A `ggplot` object

## References

For examples, see:
<https://www.open-systems-pharmacology.org/TLF-Library/articles/box-whisker-vignette.html>

## See also

Other molecule plots:
[`plotCumulativeTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotCumulativeTimeProfile.md),
[`plotDDIRatio()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotDDIRatio.md),
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
# Produce box-and-whisker plots of log-normal distributed data
boxData <- data.frame(x = c(rep("A", 500), rep("B", 500)), y = rlnorm(1000))

plotBoxWhisker(data = boxData, dataMapping = BoxWhiskerDataMapping$new(x = "x", y = "y"))


# Remove outliers from boxplot
plotBoxWhisker(
  data = boxData,
  dataMapping = BoxWhiskerDataMapping$new(x = "x", y = "y"),
  outliers = FALSE
)

```
