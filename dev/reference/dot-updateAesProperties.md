# .updateAesProperties

Updates the aesthetic properties of `plotObject`

## Usage

``` r
.updateAesProperties(
  plotObject,
  plotConfigurationProperty,
  propertyNames,
  data,
  mapLabels
)
```

## Arguments

- plotObject:

  A `ggplot` object

- plotConfigurationProperty:

  `PlotConfiguration` property name included in . `"points"`, `"lines"`,
  `"ribbons"` or `"errorbars"`

- propertyNames:

  Names of aesthetic property (e.g. `"color"`, `"shape"`...)

- data:

  A data.frame with labels mapped to properties and obtained from a
  `DataMapping` object

- mapLabels:

  List of mapped label names passed to
  [`ggplot2::aes`](https://ggplot2.tidyverse.org/reference/aes.html)

## Value

A `ggplot` object
