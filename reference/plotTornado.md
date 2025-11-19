# plotTornado

Producing tornado plots

## Usage

``` r
plotTornado(
  data = NULL,
  metaData = NULL,
  x = NULL,
  y = NULL,
  sorted = NULL,
  colorPalette = NULL,
  bar = TRUE,
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

  Character values to plot along the `y` axis. Only used instead of
  `data` if `data` is `NULL`.

- sorted:

  Optional logical value defining if `y` values are sorted by absolute
  values of `x`.

- colorPalette:

  Optional character values defining a `ggplot2` colorPalette (e.g.
  `"Spectral"`)

- bar:

  Optional logical value setting tornado plot as bar plot instead of
  scatter plot.

- dataMapping:

  A `TornadoDataMapping` object mapping `x`, `y` and aesthetic groups to
  their variable names of `data`.

- plotConfiguration:

  An optional `TornadoPlotConfiguration` object defining labels, grid,
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
[`plotQQ()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotQQ.md),
[`plotResVsPred()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotResVsPred.md),
[`plotResVsTime()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotResVsTime.md),
[`plotSimulatedTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotSimulatedTimeProfile.md),
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/plotTimeProfile.md)

## Examples

``` r
# Produce a tornado plot
plotTornado(x = c(2, -1, 3), y = c("A", "B", "C"))


# Produce a tornado plot as scatter plot
plotTornado(x = c(2, -1, 3), y = c("A", "B", "C"), bar = FALSE)


# Produce a tornado plot as is (no sorting)
plotTornado(x = c(2, -1, 3), y = c("A", "B", "C"), sorted = FALSE)

```
