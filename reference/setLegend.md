# setLegend

Set legend position, title, font and/or caption

## Usage

``` r
setLegend(
  plotObject,
  position = NULL,
  title = NULL,
  font = NULL,
  caption = NULL
)
```

## Arguments

- plotObject:

  Graphical object created from ggplot

- position:

  legend position. Use enum `LegendPositions` to access the list of
  legend positions.

- title:

  character or `Label` object

- font:

  `Font` object defining legend font

- caption:

  data.frame containing the caption properties of the legend

## Value

A `ggplot` object
