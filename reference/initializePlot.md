# initializePlot

Initialize a `ggplot` object and set its labels, grid, background and
watermark

## Usage

``` r
initializePlot(plotConfiguration = NULL)
```

## Arguments

- plotConfiguration:

  An optional `PlotConfiguration` object defining labels, grid,
  background and watermark

## Value

A `ggplot` graphical object

## References

For examples, see:
<https://www.open-systems-pharmacology.org/TLF-Library/articles/atom-plots.html>

## See also

Other atom plots:
[`addErrorbar()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/addErrorbar.md),
[`addLine()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/addLine.md),
[`addRibbon()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/addRibbon.md),
[`addScatter()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/addScatter.md)

## Examples

``` r
# Initialize an empty plot
p <- initializePlot()

# Implement a customized configuration using PlotConfiguration
config <- PlotConfiguration$new(title = "My Plot", xlabel = "x variable", ylabel = "y variable")
p <- initializePlot(config)
```
