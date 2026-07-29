# .getAestheticValuesFromConfiguration

Get list of values for requested aesthetic property

## Usage

``` r
.getAestheticValuesFromConfiguration(
  n = 1,
  position = 0,
  plotConfigurationProperty,
  propertyNames
)
```

## Arguments

- n:

  integer defining size of returned aesthetic vector

- position:

  integer defining the current position in the aesthetic map

- plotConfigurationProperty:

  `PlotConfiguration` property name included in . `"points"`, `"lines"`,
  `"ribbons"` or `"errorbars"`

- propertyNames:

  Names of aesthetic property (e.g. `"color"`, `"shape"`...)

- plotObject:

  A `ggplot` object

## Value

A list of values for requested aesthetic property
