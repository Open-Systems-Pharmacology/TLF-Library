# setWatermark

Set the watermark of a `ggplot` object. Unlike `addWatermark`, the
watermark layer is overridden by `setWatermark`.

## Usage

``` r
setWatermark(
  plotObject,
  watermark = NULL,
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
p <- initializePlot()
setWatermark(p, "watermark")


# Watermark with font properties
watermarkLabel <- Label$new(text = "watermark", color = "blue")
setWatermark(p, watermarkLabel)


# Horizontal watermark
setWatermark(p, watermarkLabel, angle = 0)


# Watermark totally opaque
setWatermark(p, watermarkLabel, alpha = 1)

```
