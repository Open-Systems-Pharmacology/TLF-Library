# .addLLOQLayer

Add a line layer representing the Lower Limit Of Quantification (LLOQ)

## Usage

``` r
.addLLOQLayer(plotObject, data, mapLabels, direction)
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

- direction:

  Whether to draw LLOQ lines for x (vertical), y (horizontal) or x and y
  (both).

## Value

A `ggplot` object
