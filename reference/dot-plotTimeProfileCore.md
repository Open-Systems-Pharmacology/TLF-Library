# .plotTimeProfileCore

Producing Core of Time Profile plots

## Usage

``` r
.plotTimeProfileCore(
  data = NULL,
  metaData = NULL,
  dataMapping = NULL,
  observedData = NULL,
  observedDataMapping = NULL,
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

- dataMapping:

  A `TimeProfileDataMapping` object mapping `x`, `y`, `ymin`, `ymax` and
  aesthetic groups to their variable names of `data`.

- observedData:

  A data.frame to use for plot. Unlike `data`, meant for simulated data,
  plotted as lines and ribbons; `observedData` is plotted as scatter
  points and errorbars.

- observedDataMapping:

  An `ObservedDataMapping` object mapping `x`, `y`, `ymin`, `ymax` and
  aesthetic groups to their variable names of `observedData`.

- plotConfiguration:

  An optional `TimeProfilePlotConfiguration` object defining labels,
  grid, background and watermark.

- plotObject:

  An optional `ggplot` object on which to add the plot layer

## Value

A `ggplot` object
