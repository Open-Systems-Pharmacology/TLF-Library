# Create a plot grid

Create a plot grid using the
[`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
function. The required arguments are supplied through the
`PlotGridConfiguration` object.

## Usage

``` r
plotGrid(plotGridConfiguration)
```

## Arguments

- plotGridConfiguration:

  A `PlotGridConfiguration` object, which is an `R6` class object that
  defines properties of a plot grid (like number of rows, columns,
  labels, etc.).

## References

For more, see:
<https://patchwork.data-imaginist.com/articles/patchwork.html>

## See also

Other molecule plots:
[`plotBoxWhisker()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotBoxWhisker.md),
[`plotCumulativeTimeProfile()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotCumulativeTimeProfile.md),
[`plotDDIRatio()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/plotDDIRatio.md),
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

library(ggplot2)
library(tlf)

# only `{tlf}` ---------------------

# plots to be arranged in a grid
set.seed(123)
ls_plots <- list(
  plotHistogram(x = rnorm(100)),
  plotHistogram(x = rnorm(100, mean = 3)),
  plotHistogram(x = rnorm(100, mean = 10))
)
#> Warning: Ignoring unknown parameters: `size`
#> Warning: Ignoring unknown parameters: `size`
#> Warning: Ignoring unknown parameters: `size`

# create an instance of plot configuration class
plotGridObj <- PlotGridConfiguration$new(plotList = ls_plots)

# specify further customizations for the plot grid
plotGridObj$title <- "my combined plot"
plotGridObj$subtitle <- "something clever"
plotGridObj$caption <- "my sources"
plotGridObj$nColumns <- 2L
plotGridObj$tagLevels <- "A"
plotGridObj$tagPrefix <- "Plot ("
plotGridObj$tagSuffix <- ")"
plotGridObj$tagColor <- "blue"
plotGridObj$tagSize <- 15
plotGridObj$tagAngle <- 45
plotGridObj$tagPosition <- TagPositions$top
plotGridObj$titleHorizontalJustification <- HorizontalJustification$middle
plotGridObj$subtitleHorizontalJustification <- HorizontalJustification$middle

# plot the grid
plotGrid(plotGridObj)


#  `{tlf}` and `{ggplot2}` ---------------------

# `{tlf}` plot
set.seed(123)
p1 <- plotBoxWhisker(mtcars,
  dataMapping = BoxWhiskerDataMapping$new(x = "am", y = "wt"), outliers = FALSE
)

# custom `{ggplot2}` plot
set.seed(123)
p2 <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

# create an instance of plot configuration class
plotGridObj2 <- PlotGridConfiguration$new(list(p1, p2))

# specify further customizations for the plot grid
plotGridObj2$nColumns <- 1L
plotGridObj2$tagLevels <- "i"

# plot the grid
plotGrid(plotGridObj2)

```
