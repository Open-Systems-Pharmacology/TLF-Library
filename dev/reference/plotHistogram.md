# plotHistogram

Producing Histograms

## Usage

``` r
plotHistogram(
  data = NULL,
  metaData = NULL,
  x = NULL,
  dataMapping = NULL,
  frequency = NULL,
  bins = NULL,
  binwidth = NULL,
  stack = NULL,
  distribution = NULL,
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

- dataMapping:

  A `HistogramDataMapping` object mapping `x` and aesthetic groups to
  their variable names of `data`.

- frequency:

  logical defining if histogram displays a frequency in y axis

- bins:

  Number or edges of bins. If `bins` is provided as a single numeric
  values, `bin` corresponds to number of bins. The bin edges are then
  equally spaced within the range of data. If `bins` is provided as an
  array of numeric values, `bin` corresponds to their edges. Default
  value, `bins=NULL`, uses the value defined by `dataMapping`

- binwidth:

  Numerical value of defining the width of each bin. If defined,
  `binwidth` can overwrite `bins` if `bins` was not provided or simply
  provided as a single value. Default value, `binwidth=NULL`, uses the
  value defined by `dataMapping`

- stack:

  Logical defining for multiple histograms if their bars are stacked
  Default value, `stack=NULL`, uses the value defined by `dataMapping`

- distribution:

  Name of distribution to fit to the data. Only 2 distributions are
  currently available: `"normal"` and `"logNormal"` Use
  `distribution="none"` to prevent fit of distribution Default value,
  `distribution=NULL`, uses the value defined by `dataMapping`

- plotConfiguration:

  An optional `HistogramPlotConfiguration` object defining labels, grid,
  background and watermark.

- plotObject:

  An optional `ggplot` object on which to add the plot layer

## Value

A `ggplot` object

## References

For examples, see:
<https://www.open-systems-pharmacology.org/TLF-Library/articles/histogram.html>

## See also

Other molecule plots:
[`plotBoxWhisker()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotBoxWhisker.md),
[`plotCumulativeTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotCumulativeTimeProfile.md),
[`plotDDIRatio()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotDDIRatio.md),
[`plotGrid()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotGrid.md),
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
# Produce histogram of normally distributed data
plotHistogram(x = rnorm(100))
#> Warning: Ignoring unknown parameters: `size`


# Produce histogram of normally distributed data normalized in y axis
plotHistogram(x = rnorm(100), frequency = TRUE)
#> Warning: Ignoring unknown parameters: `size`


# Produce histogram of normally distributed data with many bins
plotHistogram(x = rlnorm(100), bins = 21)
#> Warning: Ignoring unknown parameters: `size`


# Produce histogram of fitted normally distributed data
plotHistogram(x = rlnorm(100), distribution = "normal")
#> Warning: Ignoring unknown parameters: `size`



# Produce histogram of fitted normally distributed data
plotHistogram(x = rlnorm(100), distribution = "normal", frequency = TRUE, stack = TRUE)
#> Warning: Ignoring unknown parameters: `size`

```
