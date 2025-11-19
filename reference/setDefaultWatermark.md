# setDefaultWatermark

Set default watermark value for current theme

## Usage

``` r
setDefaultWatermark(watermark = NULL)
```

## Arguments

- watermark:

  A character value or `Label` object

## Examples

``` r
# Set default watermark using a character
setDefaultWatermark("Confidential")
addScatter(x = c(1, 2, 1, 2, 3), y = c(5, 0, 2, 3, 4))


# Set default watermark using a `Label` object
setDefaultWatermark(Label$new(text = "Confidential", color = "red", angle = 30))
addScatter(x = c(1, 2, 1, 2, 3), y = c(5, 0, 2, 3, 4))

```
