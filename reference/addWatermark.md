# addWatermark

Creates a `ggplot` grob based on the label text and its font properties.
Then, adds the grob to the `ggplot` object as a new layer using
[`ggplot2::annotation_custom`](https://ggplot2.tidyverse.org/reference/annotation_custom.html).

## Usage

``` r
addWatermark(
  plotObject,
  watermark,
  color = NULL,
  size = NULL,
  angle = NULL,
  alpha = NULL
)
```

## Arguments

- plotObject:

  A `ggplot` object

- watermark:

  A character value or a `Label` object

- color:

  Color of the watermark.

- size:

  Size of the watermark.

- angle:

  Angle of the watermark (in degree).

- alpha:

  Numeric value between 0 and 1 corresponding to transparency of the
  watermark The closer to 0, the more transparent the watermark is. The
  closer to 1, the more opaque the watermark is.

## Value

A `ggplot` object

## Examples

``` r
# Add a watermark to an empty plot
p <- ggplot2::ggplot()
addWatermark(p, "watermark")


# Watermark with font properties
watermarkLabel <- Label$new(text = "watermark", color = "blue")
addWatermark(p, watermarkLabel)


# Horizontal watermark
addWatermark(p, watermarkLabel, angle = 0)


# Watermark totally opaque
addWatermark(p, watermarkLabel, alpha = 1)


# As multiple layers of watermark:
p2 <- addWatermark(p, watermarkLabel, alpha = 1)
addWatermark(p2, "other watermark", color = "red", angle = 90)

```
