# .addErrorbarLayer

Add errorbar layer of a molecule plot

## Usage

``` r
.addErrorbarLayer(plotObject, data, mapLabels, direction = "vertical")
```

## Arguments

- plotObject:

  A `ggplot` object

- data:

  A data.frame with labels mapped to properties and obtained from a
  `DataMapping` object

- mapLabels:

  List of mapped label names passed to
  [`ggplot2::aes_string`](https://ggplot2.tidyverse.org/reference/aes_.html)

## Value

A `ggplot` object
