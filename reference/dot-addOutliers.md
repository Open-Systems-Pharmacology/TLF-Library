# .addOutliers

Add scatter points for outliers to `ggplot` object

## Usage

``` r
.addOutliers(data, metaData, dataMapping, plotConfiguration, plotObject)
```

## Arguments

- data:

  A data.frame to use for plot.

- metaData:

  A named list of information about `data` such as the `dimension` and
  `unit` of its variables.

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
