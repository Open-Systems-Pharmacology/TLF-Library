# .setPlotConfiguration

Set `PlotConfiguration` object internally using `tlf` default if
`plotConfiguration` is not provided

## Usage

``` r
.setPlotConfiguration(
  plotConfiguration,
  PlotConfigurationClass,
  data = NULL,
  metaData = NULL,
  dataMapping = NULL
)
```

## Arguments

- plotConfiguration:

  A `PlotConfigurationClass` object

- PlotConfigurationClass:

  Required class for `plotConfiguration`

- data:

  A data.frame potentially used for smart plot configuration

- metaData:

  A list of meta data potentially used for smart plot configuration

- dataMapping:

  A `DataMapping` object potentially used for smart plot configuration

## Value

A `PlotConfiguration` object
