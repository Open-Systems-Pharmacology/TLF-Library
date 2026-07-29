# getPKRatioMeasure

Get a summary table of PK Ratio within specific limits

## Usage

``` r
getPKRatioMeasure(data, dataMapping = NULL, ratioLimits = c(1.5, 2))
```

## Arguments

- data:

  A data.frame to use for plot.

- dataMapping:

  A `PKRatioDataMapping` object mapping `x`, `y` and aesthetic groups to
  their variable names of `data`.

- ratioLimits:

  Numeric positive values limits

## Value

A data.frame of summary of PK Ratio within specific limits

## Examples

``` r
# Get summary of usual PK Ratio limits
pkData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))

getPKRatioMeasure(data = pkData, dataMapping = PKRatioDataMapping$new(x = "x", y = "y"))
#>                        Number Ratio
#> Points Total                5    NA
#> Points within 1.5-fold      0   0.0
#> Points within 2-fold        1   0.2

# Get summary of other PK Ratio limits
getPKRatioMeasure(
  data = pkData,
  dataMapping = PKRatioDataMapping$new(x = "x", y = "y"),
  ratioLimits = seq(1.5, 5, 0.5)
)
#>                        Number Ratio
#> Points Total                5    NA
#> Points within 1.5-fold      0   0.0
#> Points within 2-fold        1   0.2
#> Points within 2.5-fold      1   0.2
#> Points within 3-fold        2   0.4
#> Points within 3.5-fold      2   0.4
#> Points within 4-fold        3   0.6
#> Points within 4.5-fold      3   0.6
#> Points within 5-fold        5   1.0
```
