# setPlotLabels

Set labels properties on a ggplot object

## Usage

``` r
setPlotLabels(
  plotObject,
  title = NULL,
  subtitle = NULL,
  xlabel = NULL,
  ylabel = NULL,
  caption = NULL
)
```

## Arguments

- plotObject:

  A `ggplot` object

- title:

  A character value or `Label` object

- subtitle:

  A character value or `Label` object

- xlabel:

  A character value or `Label` object

- ylabel:

  A character value or `Label` object

- caption:

  A character value or `Label` object

## Value

A `ggplot` object

## Examples

``` r
# Set labels of a scatter plot
p <- addScatter(x = c(1, 2, 1, 2, 3), y = c(5, 0, 2, 3, 4))

setPlotLabels(p, xlabel = "new x label", ylabel = "new y label")


# Set labels using Label object
setPlotLabels(p, ylabel = Label$new(text = "red y label", color = "red"))

```
