# setYAxis

Set Y-axis properties of a `ggplot` object

## Usage

``` r
setYAxis(
  plotObject,
  scale = NULL,
  valuesLimits = NULL,
  axisLimits = NULL,
  limits = lifecycle::deprecated(),
  ticks = NULL,
  ticklabels = NULL,
  minorTicks = NULL,
  font = NULL,
  expand = NULL
)
```

## Arguments

- plotObject:

  A `ggplot` object to set X-axis properties

- scale:

  Scale of axis. Use enum `Scaling` to access names of scales.

- valuesLimits:

  Optional numeric values of values limits

- axisLimits:

  Optional numeric values of axis limits

- limits:

  **\[deprecated\]**. Replaced by axisLimits argument.

- ticks:

  Optional values or function for axis ticks

- ticklabels:

  Optional values or function for axis ticklabels

- minorTicks:

  Optional values or function for axis minor ticks

- font:

  A `Font` object defining font of ticklabels

- expand:

  Logical defining if data is expanded until axis

## Value

A `ggplot` object

## Examples

``` r
myPlot <- addLine(x = c(1, 2, 3), y = c(10, 50, 100))

# Set y-axis in log scale
setYAxis(myPlot, scale = Scaling$log)


# Set y-axis ticklabels to Greek letters
setYAxis(myPlot, ticks = c(10, 50, 100), ticklabels = parse(text = c("alpha", "beta", "gamma")))


# Set y-axis limits
setYAxis(myPlot, axisLimits = c(10, 75))


# Set y-axis fonts
setYAxis(myPlot, font = Font$new(color = "blue", size = 14))
```
