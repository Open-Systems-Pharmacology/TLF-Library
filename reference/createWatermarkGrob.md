# createWatermarkGrob

Creates a `ggplot` grob based on the label text and its font properties.

## Usage

``` r
createWatermarkGrob(label, alpha = NULL)
```

## Arguments

- label:

  A character value or `Label` object

- alpha:

  Numeric value between 0 and 1 corresponding to transparency of the
  watermark The closer to 0, the more transparent the watermark is. The
  closer to 1, the more opaque the watermark is.

## Value

A `ggplot` grob
