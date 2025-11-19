# .getAesPropertyColumnNameFromLabels

Get the column names of the variables mapped to aesthetic properties

## Usage

``` r
.getAesPropertyColumnNameFromLabels(mapLabels, propertyNames)
```

## Arguments

- mapLabels:

  List of mapped label names passed to
  [`ggplot2::aes`](https://ggplot2.tidyverse.org/reference/aes.html)

- propertyNames:

  Names of aesthetic property (e.g. `"color"`, `"shape"`...)

## Value

A list of variable names
